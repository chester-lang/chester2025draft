'use client';

import { useTranslations } from 'next-intl';
import Link from 'next/link';

export default function GetStartedPage() {
    const t = useTranslations('GetStartedPage');

    return (
        <main className="flex-grow flex flex-col items-center justify-center p-4 pb-8 gap-8 sm:p-8">
            <div className="flex flex-col gap-6 w-full max-w-4xl">
                <h1 className="text-2xl font-bold text-center">{t('title')}</h1>
                <p className="text-center">{t('introText')}</p>

                <h2 className="text-xl font-bold mt-6">{t('step1Title')}</h2>
                <p>{t('step1Description')}</p>

                <h3 className="text-lg font-semibold mt-4">{t('linuxMacOSWSL')}</h3>
                <pre>
                    <code className="language-bash">
{`curl -fsSL https://moonrepo.dev/install/proto.sh | bash`}
                    </code>
                </pre>

                <h3 className="text-lg font-semibold mt-4">{t('windows')}</h3>
                <pre>
                    <code className="language-powershell">
{`irm https://moonrepo.dev/install/proto.ps1 | iex`}
                    </code>
                </pre>

                <h2 className="text-xl font-bold mt-6">{t('step2Title')}</h2>
                <p>{t('step2Description')}</p>

                <pre>
                    <code className="language-bash">
{`proto plugin add --global chester "https://github.com/chester-lang/chester/raw/refs/heads/main/proto.toml"
proto install chester`}
                    </code>
                </pre>
            </div>
        </main>
    );
}
