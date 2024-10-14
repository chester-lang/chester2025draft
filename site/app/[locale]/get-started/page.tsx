'use client';

import { useTranslations } from 'next-intl';
import Link from 'next/link';
import SyntaxHighlighter from 'react-syntax-highlighter';
import { docco, dark, vs2015 } from 'react-syntax-highlighter/dist/esm/styles/hljs';
import { useTheme } from '@/components/ThemeContext';

export default function GetStartedPage() {
    const t = useTranslations('GetStartedPage');
    const { theme } = useTheme();
    const syntaxStyle = theme === 'dark' ? dark : docco;

    return (
      <main className="flex-grow flex flex-col items-center justify-center p-4 pb-8 gap-8 sm:p-8">
        <div className="flex flex-col gap-6 w-full max-w-4xl">
          <h1 className="text-2xl font-bold text-center">{t('title')}</h1>
          <p className="text-center">{t('introText')}</p>

          {/* Step 1 */}
          <h2 className="text-xl font-bold mt-6">{t('step1Title')}</h2>
          <p>{t('step1Description')}</p>

          {/* Linux/MacOS/WSL Instructions */}
          <h3 className="text-lg font-semibold mt-4">{t('linuxMacOSWSL')}</h3>
          <SyntaxHighlighter language="bash" style={syntaxStyle}>
            {`curl -fsSL https://moonrepo.dev/install/proto.sh | bash`}
          </SyntaxHighlighter>

          {/* Windows Instructions */}
          <h3 className="text-lg font-semibold mt-4">{t('windows')}</h3>
          <SyntaxHighlighter language="powershell" style={syntaxStyle}>
            {`irm https://moonrepo.dev/install/proto.ps1 | iex`}
          </SyntaxHighlighter>

          {/* Step 2 */}
          <h2 className="text-xl font-bold mt-6">{t('step2Title')}</h2>
          <p>{t('step2Description')}</p>

          <SyntaxHighlighter language="bash" style={syntaxStyle}>
            {`proto plugin add --global chester "https://github.com/chester-lang/chester/raw/refs/heads/main/proto.toml"
proto install chester`}
          </SyntaxHighlighter>
        </div>
      </main>
    );
  }