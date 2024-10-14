import { getRequestConfig } from 'next-intl/server';
import { getMessages } from '@/i18n';

export default getRequestConfig(async () => {
  let locale = 'en-nz';

  return {
    locale: locale,
    messages: await getMessages(locale),
  };
});
