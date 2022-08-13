export function blurOnKey(event) {
  if (/^Enter|Escape|Tab$/.test(event.code)) { event.target.blur() }
}
