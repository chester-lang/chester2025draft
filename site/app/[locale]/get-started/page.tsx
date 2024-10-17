'use client';

import { useTranslations } from 'next-intl';
import Link from 'next/link';
import SyntaxHighlighter from 'react-syntax-highlighter';
import {
  docco,
  dark,
  vs2015,
} from 'react-syntax-highlighter/dist/esm/styles/hljs';
import { useTheme } from '@/components/ThemeContext';
import { useState, useEffect } from 'react';

export default function GetStartedPage() {
  const t = useTranslations('GetStartedPage');
  const { theme } = useTheme();
  const syntaxStyle = theme === 'dark' ? dark : docco;
  const [selectedOS, setSelectedOS] = useState('unix');

  useEffect(() => {
    // Detect user's OS
    const userAgent = window.navigator.userAgent.toLowerCase();
    if (userAgent.indexOf('win') > -1) {
      setSelectedOS('windows');
    } else {
      setSelectedOS('unix');
    }
  }, []);

  const installCommand =
    selectedOS === 'unix'
      ? `curl -fsSL https://github.com/chester-lang/chester/raw/refs/heads/main/i.sh | bash`
      : `irm https://github.com/chester-lang/chester/raw/refs/heads/main/i.ps1 | iex`;

  return (
    <main className="flex-grow flex flex-col items-center justify-center p-4 pb-8 gap-8 sm:p-8">
      <div className="flex flex-col gap-6 w-full max-w-4xl">
        <h1 className="text-2xl font-bold text-center">{t('title')}</h1>
        <p className="text-center">{t('introText')}</p>

        <div className="flex justify-center items-center gap-4">
          <button
            onClick={() => setSelectedOS('unix')}
            className={`px-4 py-2 rounded ${selectedOS === 'unix' ? 'bg-blue-500 text-white' : 'bg-gray-200 text-black'}`}
          >
            Linux/macOS/WSL
          </button>
          <button
            onClick={() => setSelectedOS('windows')}
            className={`px-4 py-2 rounded ${selectedOS === 'windows' ? 'bg-blue-500 text-white' : 'bg-gray-200 text-black'}`}
          >
            Windows
          </button>
        </div>

        <SyntaxHighlighter
          language={selectedOS === 'unix' ? 'bash' : 'powershell'}
          style={syntaxStyle}
        >
          {installCommand}
        </SyntaxHighlighter>

        <h2 className="text-xl font-bold mt-6">{t('manualInstallTitle')}</h2>
        <Link
          href="https://moonrepo.dev/docs/proto/install"
          className="text-blue-500 hover:underline"
          target="_blank"
          rel="noopener noreferrer"
        >
          {t('installProtoLink')}
        </Link>
        <p>{t('manualInstallDescription')}</p>

        <SyntaxHighlighter language="bash" style={syntaxStyle}>
          {`proto plugin add --global chester "https://github.com/chester-lang/chester/raw/refs/heads/main/proto.toml"
proto install chester`}
        </SyntaxHighlighter>
      </div>
    </main>
  );
}
