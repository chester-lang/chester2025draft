'use client';

import { useTranslations } from 'next-intl';
import Link from 'next/link';

export default function CommunityPage() {
  const t = useTranslations('CommunityPage');

  return (
    <main className="flex-grow flex flex-col items-center justify-center p-4 pb-8 gap-8 sm:p-8">
      <h1 className="text-2xl font-bold text-center">{t('title')}</h1>
      <p className="text-center">{t('introText')}</p>
      <Link
        href="https://discord.gg/QaCpFkjUES"
        className="mt-4 text-blue-500 hover:underline"
        target="_blank"
        rel="noopener noreferrer"
      >
        {t('joinDiscord')}
      </Link>
    </main>
  );
}
