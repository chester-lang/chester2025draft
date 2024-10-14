'use client';

import { redirect } from 'next/navigation';
import { getPreferredLocale } from '@/i18n/localeUtils';

export default function RootPage() {
  const preferredLocale = getPreferredLocale();
  redirect(`/${preferredLocale}`);
}
