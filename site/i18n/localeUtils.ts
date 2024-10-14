'use client';
import { SUPPORTED_LOCALES, SupportedLocale, DEFAULT_LOCALE } from './index';
import { DOCS_LOCALES, DEFAULT_DOCS_LOCALE, DocsLocale } from './index';
import { useLocale } from 'next-intl';

function findMatchingLocale(
  availableLocales: readonly string[],
  preferredLocales: readonly string[]
): string | undefined {
  for (const lang of preferredLocales) {
    // First, try to find an exact match
    const exactMatch = availableLocales.find(
      (locale) => locale.toLowerCase() === lang.toLowerCase()
    );
    if (exactMatch) {
      return exactMatch;
    }

    // If no exact match, try to find a match for the language part
    const langPart = lang.split('-')[0].toLowerCase();
    const languageMatch = availableLocales.find((locale) =>
      locale.toLowerCase().startsWith(langPart)
    );
    if (languageMatch) {
      return languageMatch;
    }
  }
  return undefined;
}

export function getPreferredLocale(
  availableLocales: string[] = SUPPORTED_LOCALES
): SupportedLocale {
  if (typeof navigator === 'undefined') {
    return DEFAULT_LOCALE;
  }

  const preferredLocales = navigator.languages || [navigator.language];
  const matchedLocale = findMatchingLocale(availableLocales, preferredLocales);

  return (matchedLocale as SupportedLocale) || DEFAULT_LOCALE;
}

export function useDocsUrl(path: string = ''): string {
  const currentLocale = useLocale();
  const docsLocale =
    findMatchingLocale(DOCS_LOCALES, [currentLocale]) || DEFAULT_DOCS_LOCALE;
  const baseUrl =
    docsLocale === DEFAULT_DOCS_LOCALE ? '/docs' : `/docs/${docsLocale}`;
  return `${baseUrl}${path}`;
}
