'use client';

import { useEffect, useRef } from 'react';
import { XTerm } from '@pablo-lion/xterm-react';
import { Terminal } from '@xterm/xterm';
import { useTranslations } from 'next-intl';
import { useTheme } from '@/components/ThemeContext';
import Link from 'next/link';
import { initTerminal } from './terminal';

export default function Home() {
  const t = useTranslations('Home');
  const xtermRef = useRef<any>(null);
  const { theme } = useTheme();

  useEffect(() => {
    if (xtermRef.current) {
      const terminal = xtermRef.current.terminal as Terminal;
      initTerminal(terminal);
    }
  }, [xtermRef]);

  useEffect(() => {
    if (xtermRef.current) {
      const terminal = xtermRef.current.terminal as Terminal;
      // Apply theme to XTerm
      if (theme === 'dark') {
        terminal.options.theme = {};
      } else if (theme === 'light') {
        // Color source: https://github.com/microsoft/vscode/blob/main/extensions/theme-defaults/themes/light_plus.json
        terminal.options.theme = {
          background: '#ffffff',
          foreground: '#333333',
          cursor: '#333333',
          cursorAccent: '#ffffff',
          selectionBackground: '#add6ff',
          //overviewRulerBorder: '#aaaaaa',
          black: '#000000',
          blue: '#0451a5',
          brightBlack: '#666666',
          brightBlue: '#0451a5',
          brightCyan: '#0598bc',
          brightGreen: '#14ce14',
          brightMagenta: '#bc05bc',
          brightRed: '#cd3131',
          brightWhite: '#a5a5a5',
          brightYellow: '#b5ba00',
          cyan: '#0598bc',
          green: '#00bc00',
          magenta: '#bc05bc',
          red: '#cd3131',
          white: '#555555',
          yellow: '#949800',
        };
      }
      // Force a redraw of the terminal
      terminal.refresh(0, terminal.rows - 1);
    }
  }, [theme, xtermRef]);

  return (
    <div className="flex flex-col bg-white dark:bg-gray-900 text-black dark:text-white">
      <div className="flex-grow flex flex-col items-center justify-start p-1 sm:p-2">
        <main className="flex flex-col gap-2 w-full max-w-4xl mt-2 sm:mt-4">
          <h1 className="text-2xl font-bold text-center">{t('title')}</h1>
          <div className="text-center mb-4 flex flex-col sm:flex-row justify-center gap-2">
            <Link
              href="https://marketplace.visualstudio.com/items?itemName=mio-19.chester-language-support"
              className="text-blue-500 hover:underline"
              target="_blank"
              rel="noopener noreferrer"
            >
              {t('vsCodeExtensionText')}
            </Link>
            <Link
              href="https://plugins.jetbrains.com/plugin/25508-chester-language-support/"
              className="text-blue-500 hover:underline"
              target="_blank"
              rel="noopener noreferrer"
            >
              {t('ideaPluginText')}
            </Link>
          </div>
          <XTerm ref={xtermRef} />
        </main>
      </div>
    </div>
  );
}
