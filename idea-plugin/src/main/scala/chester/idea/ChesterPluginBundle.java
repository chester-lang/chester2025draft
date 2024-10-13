// Copyright 2000-2020 JetBrains s.r.o. and other contributors. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package chester.idea;
import com.intellij.DynamicBundle;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.PropertyKey;
public class ChesterPluginBundle extends DynamicBundle {
    @NonNls
    private static final String BUNDLE = "messages.ChesterPluginBundle";
    private static final ChesterPluginBundle INSTANCE = new ChesterPluginBundle();
    private ChesterPluginBundle() {
        super(BUNDLE);
    }
    @Nls
    public static String message(@NotNull @PropertyKey(resourceBundle = BUNDLE) String key, @NotNull Object... params) {
        return INSTANCE.getMessage(key, params);
    }
}