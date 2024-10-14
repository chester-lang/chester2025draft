import { Terminal } from '@xterm/xterm';
import { Readline } from 'xterm-readline';

export function startRepl(terminal: Terminal): Promise<void>;
/* eslint-disable @typescript-eslint/no-explicit-any */
export function startReplPty(pty: any): Promise<void>;
export function startReplReadline(rl: Readline): Promise<void>;
export function runFile(content: string, lightMode: boolean): Promise<string>;
