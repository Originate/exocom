// expectFn should throw on failure, should not throw to success
export async function condition(expectFn, timeout) {
  const startTimestamp = Date.now()
  try {
    await expectFn()
  } catch (error) {
    await new Promise(resolve => setTimeout(resolve, 1))
    const timeChange = Date.now() - startTimestamp
    if (timeout - timeChange < 0) {
      throw error
    }
    await condition(expectFn, timeout - timeChange)
  }
}
