import { startRepl, startReplPty, startReplReadline } from '@/scala/main';
import { Terminal } from '@xterm/xterm';
import { Readline } from 'xterm-readline';

// https://stackoverflow.com/questions/66096260/why-am-i-getting-referenceerror-self-is-not-defined-when-i-import-a-client-side/66100185#66100185
export async function initTerminal(terminal: Terminal) {
  const { WebLinksAddon } = await import('@xterm/addon-web-links');
  terminal.loadAddon(new WebLinksAddon());
  const { FitAddon } = await import('@xterm/addon-fit');
  const fitAddon = new FitAddon();
  terminal.loadAddon(fitAddon);
  fitAddon.fit();
  terminal.focus();
  if (false) {
    const { SearchAddon } = await import('@xterm/addon-search');
    const searchAddon = new SearchAddon();
    terminal.loadAddon(searchAddon);
  }
  const used: string = 'startReplReadline';
  if (used === 'startRepl') {
    while (true) {
      try {
        await startRepl(terminal);
      } catch (e) {
        console.log(e);
      }
    }
  } else if (used === 'startReplPty') {
    const { openpty } = await import('xterm-pty');
    const { master, slave } = openpty();
    terminal.loadAddon(master);
    while (true) {
      try {
        await startReplPty(slave);
      } catch (e) {
        console.log(e);
      }
    }
  } else if (used === 'startReplReadline') {
    const rl = new Readline();
    terminal.loadAddon(rl);
    while (true) {
      try {
        await startReplReadline(rl);
      } catch (e: any) {
        console.log(e);
        rl.println(e.toString());
      }
    }
  }
}
