// components/LocaleSwitcher.tsx

'use client';
import { useLocale } from 'next-intl';
import { usePathname, useRouter } from '@/i18n/navigation';
import { SUPPORTED_LOCALES, SupportedLocale, LOCALE_NAMES } from '@/i18n';

export default function LocaleSwitcher() {
  const locale = useLocale();
  const router = useRouter();
  const pathname = usePathname();

  const onLocaleChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    const newLocale = e.target.value;
    router.replace(pathname, { locale: newLocale as SupportedLocale });
  };

  return (
    <select
      defaultValue={locale}
      onChange={onLocaleChange}
      className="bg-white text-black dark:bg-gray-800 dark:text-white border border-gray-300 dark:border-gray-600 rounded-md px-2 py-1"
    >
      {SUPPORTED_LOCALES.map((lang) => (
        <option
          key={lang}
          value={lang}
          className="bg-white text-black dark:bg-gray-800 dark:text-white"
        >
          {LOCALE_NAMES[lang]}
        </option>
      ))}
    </select>
  );
}
