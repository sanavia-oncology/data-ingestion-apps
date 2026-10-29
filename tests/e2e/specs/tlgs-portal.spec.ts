import { test, expect, type Page, type APIRequestContext } from '@playwright/test';
import { cpSync, readFileSync, renameSync } from 'node:fs';
import { join } from 'node:path';

// Serial walk through the local tlgs-portal app on a scratch copy of Kwame's demo fixture (E2E_DATA_DIR):
// build on start → publish a sheet → rebuild → drop a well → rebuild → undrop → rebuild.
const DATA = process.env.E2E_DATA_DIR!;
const DROP = join(DATA, '00-drop-folder', 'to-drop-wells_cummulative.csv');
const SOURCE = join(DATA, 'Remainder_3', '2026-09-28_CF-SANA01-clin-candidates');
const PUBLISHED = join(DATA, 'Remainder_3', '2026-11-01_E2E-published');
const ORIGINAL_DROP = readFileSync(DROP, 'utf8');

const rows = (file: string) => readFileSync(file, 'utf8').split(/\r?\n/).filter(Boolean).length - 1;
const num = (s: string | null) => Number((s ?? '').replace(/[^0-9]/g, ''));

async function manifest(request: APIRequestContext) {
  const r = await request.get('/data/manifest.json');
  expect(r.ok()).toBeTruthy();
  const m = await r.json();
  return { ...m, wells: Object.fromEntries(Object.entries(m.studies).map(([k, v]: any) => [k, v.wells])) };
}

test.describe.configure({ mode: 'serial' });

let page: Page;
const errors: string[] = [];
let version = '';
let sana03Cards = 0;

test.beforeAll(async ({ browser }) => {
  page = await browser.newPage();
  page.on('pageerror', (e) => errors.push(e.message));
  page.on('console', (m) => { if (m.type() === 'error' && !m.text().includes('favicon.ico')) errors.push(m.text()); });
});
test.afterAll(async () => { await page.close(); });

const portal = () => page.frameLocator('#portal');
const card = (code: string) => portal().locator(`#studyCards .study[data-proj="${code}"] .study-n`);
const waitForVersion = (request: APIRequestContext) =>
  expect.poll(async () => (await manifest(request)).version, { timeout: 150_000, intervals: [2000] }).not.toBe(version);

test('builds the three study sheets on start and opens on the cover', async ({ request }) => {
  const m = await manifest(request);
  expect(m.status).toBe('ok');
  expect(m.wells).toEqual({ SANA01: 1936, SANA03: 9519, SANA04: 4832 });
  expect(m.projects).toBe(96);
  expect(m.dropped).toBe(23);
  version = m.version;

  await page.goto('/');
  await expect(portal().locator('#coverView')).toBeVisible();
  await expect(portal().locator('#selectView')).toBeHidden();
  await expect(portal().locator('#toSelect')).toBeHidden();
  await expect(portal().locator('#studyCards .study')).toHaveCount(3);
  await expect(card('SANA01')).toHaveText('1,936 wells');
  sana03Cards = num(await card('SANA03').textContent());
  expect(sana03Cards).toBeGreaterThan(0);
});

test('publishing a new gated-results sheet triggers the R script and the page reloads', async ({ request }) => {
  cpSync(SOURCE, PUBLISHED, { recursive: true });
  renameSync(join(PUBLISHED, 'gating_results', '2026-09-28_CF-SANA01-clin-candidates_gated-results.csv'),
             join(PUBLISHED, 'gating_results', '2026-11-01_E2E-published_gated-results.csv'));
  const added = rows(join(PUBLISHED, 'gating_results', '2026-11-01_E2E-published_gated-results.csv'));
  expect(added).toBe(64);

  await waitForVersion(request);
  const m = await manifest(request);
  version = m.version;
  expect(m.projects).toBe(97);
  expect(m.wells).toEqual({ SANA01: 2000, SANA03: 9519, SANA04: 4832 });
  await expect(card('SANA01')).toHaveText('2,000 wells');
  await expect(portal().locator('#coverView')).toBeVisible();
});

test('dropping a well appends to the drop file, rebuilds, and keeps the study open', async ({ request }) => {
  await portal().locator('#studyCards .study[data-proj="SANA03"]').click();
  await expect(portal().locator('#studyView')).toBeVisible();
  await expect(portal().locator('#projName')).toHaveText('SANA03');
  await expect(portal().locator('.vtabs [data-view="drop"]')).toBeVisible();

  await portal().locator('#dropMode').click();
  await expect(portal().locator('#dropMode')).toHaveText('Selecting (click to stop)');
  await portal().locator('#main svg.gplot [data-i]:not(rect)').first().click({ force: true });
  await expect(portal().locator('#dropMsg')).toHaveText('+1 well');
  await expect(portal().locator('#dropInfo')).toHaveText('To drop: 1 well');

  await portal().locator('#dropDl').click();
  await expect.poll(() => rows(DROP), { timeout: 30_000 }).toBe(24);
  await waitForVersion(request);
  const m = await manifest(request);
  version = m.version;
  expect(m.dropped).toBe(24);
  expect(m.wells.SANA03).toBe(9518);
  await expect(portal().locator('#studyView')).toBeVisible();
  await expect(portal().locator('#projName')).toHaveText('SANA03');
  await expect.poll(async () => num(await card('SANA03').textContent())).toBe(sana03Cards - 1);
});

test('removing the row from the Drop list restores the file and the counts', async ({ request }) => {
  await portal().locator('.vtabs [data-view="drop"]').click();
  await expect(portal().locator('#dlInfo')).toContainText('24 rows');
  const added = ORIGINAL_DROP.trim().split('\n').length;
  const plate = readFileSync(DROP, 'utf8').trim().split('\n')[added].split(',')[2];
  const row = portal().locator('#dropListBox tbody tr[data-dlrow]', { hasText: plate });
  await expect(row).toHaveCount(1);
  await row.locator('[data-dlr]').check();
  await portal().locator('#dlRemove').click();
  await expect(portal().locator('#dlRemove')).toContainText('Click again');
  await portal().locator('#dlRemove').click();

  await expect.poll(() => rows(DROP), { timeout: 30_000 }).toBe(23);
  expect(readFileSync(DROP, 'utf8')).toBe(ORIGINAL_DROP);
  await waitForVersion(request);
  const m = await manifest(request);
  version = m.version;
  expect(m.dropped).toBe(23);
  expect(m.wells.SANA03).toBe(9519);
  await expect.poll(async () => num(await card('SANA03').textContent())).toBe(sana03Cards);
  await expect(portal().locator('#dropInfo')).toHaveText('To drop: 0 wells');
});

test('no page errors during the run', async () => {
  expect(errors).toEqual([]);
});
