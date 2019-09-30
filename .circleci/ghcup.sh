#!/bin/sh

# safety subshell to avoid executing anything in case this script is not downloaded properly
(

: "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}"

die() {
    (>&2 printf "\\033[0;31m%s\\033[0m\\n" "$1")
    exit 2
}

edo()
{
    "$@" || die "\"$*\" failed!"
}

echo
echo "Welcome to Haskell!"
echo
echo "This will download and install the Glasgow Haskell Compiler (GHC) for "
echo "the Haskell programming language, and the Cabal build tool."
echo
echo "It will add the 'cabal', 'ghc', and 'ghcup' executables to bin directory "
echo "located at: "
echo
echo "  $GHCUP_INSTALL_BASE_PREFIX/.ghcup/bin"
echo
echo "and create the environment file $GHCUP_INSTALL_BASE_PREFIX/.ghcup/env"
echo "which you should source in your ~/.bashrc or similar to get the required"
echo "PATH components."
echo

if [ -z "${BOOTSTRAP_HASKELL_NONINTERACTIVE}" ] ; then
	printf "\\033[0;35m%s\\033[0m\\n" "To proceed with the ghcup installation press ENTER, to cancel press ctrl-c."
	printf "\\033[0;35m%s\\033[0m\\n" "Note that this script can be re-run at any given time."
	echo
	# Wait for user input to continue.
	# shellcheck disable=SC2034
fi

edo mkdir -p "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/bin

if command -V "ghcup" >/dev/null 2>&1 ; then
	if [ -z "${BOOTSTRAP_HASKELL_NO_UPGRADE}" ] ; then
		edo ghcup upgrade
	fi
else
	edo curl --silent https://gitlab.haskell.org/haskell/ghcup/raw/master/ghcup > "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/bin/ghcup
	edo chmod +x "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/bin/ghcup

	cat <<-EOF > "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/env || die "Failed to create env file"
		export PATH="\$HOME/.cabal/bin:\${GHCUP_INSTALL_BASE_PREFIX:=\$HOME}/.ghcup/bin:\$PATH"
		EOF
	# shellcheck disable=SC1090
	edo . "${GHCUP_INSTALL_BASE_PREFIX}"/.ghcup/env
fi

echo
printf "\\033[0;35m%s\\033[0m\\n" "To install and run GHC you need the following dependencies:"
echo "  $(ghcup print-system-reqs)"
echo

if [ -z "${BOOTSTRAP_HASKELL_NONINTERACTIVE}" ] ; then
	printf "\\033[0;35m%s\\033[0m\\n" "You may want to install these now, then press ENTER to proceed"
	printf "\\033[0;35m%s\\033[0m\\n" "or press ctrl-c to abort. Installation may take a while."
	echo

	# Wait for user input to continue.
	# shellcheck disable=SC2034
fi

edo ghcup --cache install

edo ghcup set
edo ghcup --cache install-cabal

edo cabal new-update

printf "\\033[0;35m%s\\033[0m\\n" ""
printf "\\033[0;35m%s\\033[0m\\n" "Installation done!"
printf "\\033[0;35m%s\\033[0m\\n" ""
echo "In order to run ghc and cabal, you need to adjust your PATH variable."
echo "You may want to source '$GHCUP_INSTALL_BASE_PREFIX/.ghcup/env' in your shell"
echo "configuration to do so (e.g. ~/.bashrc)."

if [ -e "$HOME/.bashrc" ] ; then
    printf "\\033[0;35m%s\\033[0m\\n" ""
    printf "\\033[0;35m%s\\033[0m\\n" "Detected ~/.bashrc on your system..."
    printf "\\033[0;35m%s\\033[0m\\n" "If you want ghcup to automatically fix your ~/.bashrc to include the required PATH variable"
    printf "\\033[0;35m%s\\033[0m\\n" "answer with YES and press ENTER (at your own risk)."
    printf "\\033[0;35m%s\\033[0m\\n" "Otherwise press ctrl-c to abort."
    printf "\\033[0;35m%s\\033[0m\\n" ""

    echo "source $GHCUP_INSTALL_BASE_PREFIX/.ghcup/env" >> "${HOME}/.bashrc"
fi
if [ -e "$HOME/.bash_profile" ] ; then
    printf "\\033[0;35m%s\\033[0m\\n" ""
    printf "\\033[0;35m%s\\033[0m\\n" "Detected ~/.bash_profile on your system..."
    printf "\\033[0;35m%s\\033[0m\\n" "If you want ghcup to automatically fix your ~/.bash_profile to include the required PATH variable"
    printf "\\033[0;35m%s\\033[0m\\n" "answer with YES and press ENTER (at your own risk)."
    printf "\\033[0;35m%s\\033[0m\\n" "Otherwise press ctrl-c to abort."
    printf "\\033[0;35m%s\\033[0m\\n" ""

    echo "source $GHCUP_INSTALL_BASE_PREFIX/.ghcup/env" >> "${HOME}/.bash_profile"
fi

)

# vim: tabstop=4 shiftwidth=4 expandtab

