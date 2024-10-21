'use client';
import { Link } from '@/i18n/navigation';
import Image from 'next/image';
import { useState } from 'react';
import { useTranslations } from 'next-intl';
import LocaleSwitcher from './LocaleSwitcher';
// https://stackoverflow.com/questions/65930789/how-does-next-js-basepath-works-for-images/76197261#76197261
import chesterLogo from '@/chester-logo.svg';
import NextLink from 'next/link';
import { useDocsUrl } from '@/i18n/localeUtils';
//import ThemeToggle from './ThemeToggle';

export default function Header() {
  const t = useTranslations('Header');
  const [isMenuOpen, setIsMenuOpen] = useState(false);

  return (
    <header className="flex flex-wrap items-center justify-between p-4 bg-gray-100 dark:bg-gray-800">
      <div className="flex items-center">
        <Image
          src={chesterLogo}
          alt="Chester logo"
          width={40}
          height={40}
          className="mr-4 dark:invert"
        />
        <Link href="/" className="text-xl font-bold">
          Chester
        </Link>
      </div>
      <button className="md:hidden" onClick={() => setIsMenuOpen(!isMenuOpen)}>
        {isMenuOpen ? t('close') : t('menu')}
      </button>
      <nav
        className={`w-full md:w-auto ${isMenuOpen ? 'block' : 'hidden'} md:block mt-4 md:mt-0`}
      >
        <ul className="flex flex-col md:flex-row space-y-2 md:space-y-0 md:space-x-6">
          <li>
            <Link href="/" className="hover:underline">
              {t('home')}
            </Link>
          </li>
          <li>
            <Link href="/get-started" className="hover:underline">
              {t('getStarted')}
            </Link>
          </li>
          <li>
            <Link href="/playground" className="hover:underline">
              {t('playground')}
            </Link>
          </li>
          <li>
            <NextLink href={useDocsUrl()} className="hover:underline">
              {t('documentation')}
            </NextLink>
          </li>
          <li>
            <Link href="/community" className="hover:underline">
              {t('community')}
            </Link>
          </li>
        </ul>
      </nav>
      <LocaleSwitcher />
    </header>
  );
}
