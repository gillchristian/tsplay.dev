import wait from './wait'

it('resolves a promise after the given time', () => {
  jest.useFakeTimers()

  const tenSec = 1000 * 10

  const delayed = wait(tenSec)

  jest.advanceTimersByTime(tenSec + 1)

  return delayed
})
