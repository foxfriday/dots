#!/usr/bin/env bash
#
# Tools to clone remote git repositories


### Check requirements
command -v git > /dev/null || { echo "git requirement missing." >&2; exit 1; }
command -v jq > /dev/null || { echo "jq requirement missing." >&2; exit 1; }

# change directory or error
cde () {
    cd "${1}" || { echo "Unable to enter ${1}." >&2; exit 1; }
}

# Procure system information
#
# Returns
# SYS     system type, linux, darwin
# ARCH    architecture, arm, x86_64
# PROC    architecture, using x86_64 re-brand as amd64
# PKG     system package
# see: https://askubuntu.com/questions/601553/what-is-the-difference-between-x86-64-amd64-and-64-bit
system_info () {
    local ARCHINFO
    ARCHINFO=$(uname -sm)
    case "${ARCHINFO}" in
        "Darwin arm64")
            SYS=darwin
            ARCH=arm
            PROC=arm
            PKG=""
            ;;
        "Darwin x86_64")
            SYS=darwin
            ARCH=x86_64
            PROC=amd64
            PKG=""
            ;;
        "Linux armv*")
            SYS=linux
            ARCH=arm
            PROC=arm
            PKG=""
            ;;
        "Linux x86_64")
            SYS=linux
            ARCH=x86_64
            PROC=amd64
            PKG=".deb"
            ;;
        *) echo "Unable to determine architecture ${ARCHINFO}" >&2
           exit 1
           ;;
    esac
}

# Download assets from GitHub. The variable RET contains the name
# of the download or an empty string in case of failure.
#
# Parameters
# 1 repository
# 2 architecture (e.g. linux.x86_64, amd64.deb)
get_github_asset () {
    local ASSET
    local URL
    local INFO
    local REPO="https://api.github.com/repos/${1}/releases/latest"
    RET=""
    INFO=$(mktemp)
    curl -sL "${REPO}" -o "${INFO}" ||
        { rm "${INFO}"; echo "Error acquiring info from ${1}" >&2; exit 1; }
    URL=$(jq -r '.assets[].browser_download_url' "${INFO}" | grep "${2}" | grep -v musl)
    rm "${INFO}"
    ASSET="${HOME}/Downloads/${URL##*/}"
    touch "${ASSET}" || { echo "Unable to access destination ${ASSET}." >&2; exit 1; }
    echo "Downloading ${ASSET}..."
    curl -sL "${URL}" -o "${ASSET}" ||
        { rm "${ASSET}"; echo "Error acquiring assets from ${URL}" >&2; exit 1; }
    echo "done!"
    RET="${ASSET}"
}

# Parameters
# 1 Add to PATH if not present
maybe_add_to_path () {
    local BIN="${1:-/usr/bin}"
    if [[ :"$PATH": == *:"$BIN":* ]]; then
        echo "Pyenv already in path"
        return 1
    else
        export PATH="$BIN:$PATH"
        return 0
    fi
}

# Parameters:
# 1 if starts with http leave alone, otherwise assume GitHub repository
maybe_github () {
    if [[ "$1" == http* ]]; then
        REPO="${1}"
    else
        REPO=https://github.com/"${1}".git
    fi
}

# Parameters
# 1 possible branch
# 2 repo
maybe_pick_branch () {
    if [ -n "${1}" ]; then
        BRANCH="${1}"
        return 0
    fi
    local PTRN
    if [[ "${2##*/}" == emacs.git ]]; then
        PTRN=refs/heads/emacs
    else
        PTRN=refs/heads/
    fi
    mapfile -t BRANCHES < <(git ls-remote "${2}" | grep "${PTRN}" | grep -v wip | cut -f2 | sed 's/refs\/heads\///')
    local -i N=0
    echo "Cloning ${2}"
    for B in "${BRANCHES[@]}"; do
        echo "${N}   ${B}"
        N+=1
    done
    read -p "Please select branch by number?" -r CHOICE
    if [ -n "${CHOICE##*[!0-9]*}" ] && (( CHOICE < N )); then
         BRANCH="${BRANCHES[CHOICE]}"
         return 0
    else
        echo "Invalid choice! Exiting" >&2
        return 1
    fi
}

# Parameters:
# 1 local repository
# 2 remote
# 3 branch - default main
# Return
# UPDATED=0 if nothing done
# UPDATED=1 if cloned
# UPDATED=2 if pulled
get_or_update () {
    local DESTINATION="${1}"
    local REMOTE
    local COMMON
    UPDATED=0
    if [ -d "${DESTINATION}/.git" ]; then
        cde "${DESTINATION}"
        git remote update ||  { echo "unable to update remote" >&2; exit 1; }
        LOCAL=$(git rev-parse HEAD)
        REMOTE=$(git rev-parse '@{u}')
        COMMON=$(git merge-base HEAD '@{u}')

        echo "Checking ${2}"
        if [ "${LOCAL}" = "${REMOTE}" ]; then
            echo "Installed and up to date."
            return 0
        elif [ "${LOCAL}" = "${COMMON}" ]; then
            git pull ||  { echo "unable to pull remote" >&2; exit 1; }
	    BRANCH=$(git branch --show-current)
            UPDATED=2
            return 0
        elif [ "${REMOTE}" = "${COMMON}" ]; then
            echo "Local ahead of remote. Perhaps clean it?" >&2
            return 1
        else
            echo "Local and remote diverged." >&2
            return 1
        fi
    else
        maybe_github "${2}"
        maybe_pick_branch "${3}" "${REPO}"
        git clone --branch "${BRANCH}" --depth 1 "${REPO}" "${DESTINATION}" ||
            { echo "Unable to clone ${REPO}" >&2; return 1; }
        UPDATED=1
        return 0
    fi
}
