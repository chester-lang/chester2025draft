import deepmerge from 'deepmerge';
import { notFound } from 'next/navigation';

export async function getMsg(locale: string) {
  try {
    return (await import(`../messages/${locale}.json`)).default;
  } catch (error) {
    notFound();
  }
}

export async function getMessages(locale: string) {
  let result = await getMsg(locale);
  if (locale.startsWith('zh')) {
    result = deepmerge(
      deepmerge(await getMsg('zh-tw'), await getMsg('zh-sg')),
      result
    );
  }
  if (locale == 'en-nz') {
    return result;
  }
  return deepmerge(await getMsg('en-nz'), result);
}

export const LOCALE_NAMES = {
  'en-nz': 'English',
  'en-us': 'English US',
  fr: 'Français',
  'zh-tw': '漢語',
  'zh-sg': '中文',
  de: 'Deutsch',
  mi: 'Māori',
  yue: '粵語',
  wzhu: '甌語',
  ja: '日本語',
  ko: '한국어',
} as const;

export const SUPPORTED_LOCALES = Object.keys(LOCALE_NAMES) as Array<
  keyof typeof LOCALE_NAMES
>;
export type SupportedLocale = (typeof SUPPORTED_LOCALES)[number];
export const DEFAULT_LOCALE: SupportedLocale = 'en-nz';

export const DOCS_LOCALES = ['en-NZ', 'zh-TW'] as const;
export type DocsLocale = (typeof DOCS_LOCALES)[number];
export const DEFAULT_DOCS_LOCALE: DocsLocale = 'en-NZ';
