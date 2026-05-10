import numpy as np


def xform(x: np.ndarray) -> np.ndarray:
    x = np.asarray(x, dtype=float)
    i = (np.abs(x) < 10).astype(float)
    return i * (x / 10.0) + (1 - i) * np.sign(x) * np.log10(np.abs(x) + 1)


def ixform(y: np.ndarray) -> np.ndarray:
    y = np.asarray(y, dtype=float)
    i = (np.abs(y) < 1).astype(float)
    return i * (y * 10.0) + (1 - i) * np.sign(y) * (10 ** np.abs(y) - 1)
