import { defineConfig } from '@playwright/test';

// E2E_BASE_URL and E2E_DATA_DIR are set by run.sh, which starts the app on a scratch copy of the fixture.
export default defineConfig({
  testDir: './specs',
  timeout: 240_000,
  expect: { timeout: 30_000 },
  retries: 0,
  workers: 1,
  reporter: [['list'], ['html', { open: 'never' }]],
  use: {
    baseURL: process.env.E2E_BASE_URL ?? 'http://127.0.0.1:3912',
    trace: 'retain-on-failure',
    screenshot: 'only-on-failure',
    viewport: { width: 1400, height: 900 },
  },
  projects: [{ name: 'chromium', use: { browserName: 'chromium' } }],
});
