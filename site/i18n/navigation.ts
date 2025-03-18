import { createNavigation } from 'next-intl/navigation';
import { SUPPORTED_LOCALES } from '@/i18n';
const locales = SUPPORTED_LOCALES;
export const { Link, useRouter, usePathname, redirect } =
createNavigation({ locales });
