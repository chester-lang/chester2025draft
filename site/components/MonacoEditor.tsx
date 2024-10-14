'use client';

import { Editor, OnMount, OnChange } from '@monaco-editor/react';

interface MonacoEditorProps {
  code?: string;
  onChange?: OnChange;
  onMount?: OnMount;
  theme: 'vs-dark' | 'vs-light';
}

export default function MonacoEditor({
  code,
  onChange,
  onMount,
  theme,
}: MonacoEditorProps) {
  return (
    <Editor
      height="400px"
      defaultLanguage="javascript"
      value={code}
      theme={theme}
      options={{
        minimap: { enabled: false },
        automaticLayout: true,
      }}
      onMount={onMount}
      onChange={onChange}
    />
  );
}
