'use client';

import { useTranslations } from 'next-intl';

export default function Footer() {
  const t = useTranslations('Footer');

  return (
    <footer className="bg-gray-100 dark:bg-gray-800 p-4">
      <p className="text-sm text-center text-gray-700 dark:text-gray-300">
        {t('footer', { year: new Date().getFullYear() })}
      </p>
    </footer>
  );
}
