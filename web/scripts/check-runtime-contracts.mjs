import fs from 'node:fs';

const src = fs.readFileSync(new URL('../src/main.js', import.meta.url), 'utf8');

const invalidSingleSelectorForEach = /(^|[^$])\$\((?:'[^']*'|"[^"]*"|\`[^\`]*\`)(?:,[^)]*)?\)\.forEach/gm;
const matches = [...src.matchAll(invalidSingleSelectorForEach)];

if (matches.length) {
  console.error('Invalid querySelector(...).forEach usage found. Use $$(...) for multi-element bindings.');
  for (const match of matches) {
    const line = src.slice(0, match.index).split('\n').length;
    console.error(`  main.js:${line}: ${match[0].trim()}`);
  }
  process.exit(1);
}

if (!src.includes("$$('[data-player]').forEach")) {
  console.error('Player interaction binding contract is missing.');
  process.exit(1);
}

console.log('Runtime binding contracts OK.');
