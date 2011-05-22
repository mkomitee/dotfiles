function svn_prompt_info {
    if [[ -d .svn ]]; then
        echo "$ZSH_THEME_SVN_PROMPT_PREFIX$ZSH_THEME_REPO_NAME_COLOR$(svn_get_repo_name)$(svn_dirty)$ZSH_THEME_SVN_PROMPT_SUFFIX"
    fi
}
function svn_get_repo_name {
    if [ is_svn ]; then
        svn info | sed -n 's/Repository\ Root:\ .*\///p' | read SVN_ROOT
        svn info | sed -n "s/URL:\ .*$SVN_ROOT\///p" | sed "s/\/.*$//" | read SVN_DIR
        if [ -z "$SVN_DIR" ]; then
            echo "$SVN_ROOT"
        else
            echo "$SVN_ROOT/$SVN_DIR"
        fi
    fi
}
