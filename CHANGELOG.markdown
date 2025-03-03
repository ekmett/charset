0.3.12 [2025.03.03]
-------------------
* Support building with `containers-0.8.*`.
* Drop support for pre-`0.4.2.0` versions of `containers`.

0.3.11 [2024.12.04]
-------------------
* Drop support for pre-8.0 versions of GHC.

0.3.10 [2023.08.06]
-------------------
* Allow building with `bytestring-0.12.*`.
* Replace a use of `bytestring`'s `memset` function (which is now deprecated as
  of `bytestring-0.12.*`) with `base`'s `fillBytes` function.

0.3.9 [2021.11.01]
------------------
* Allow building with GHC 9.2.

0.3.8 [2021.02.17]
------------------
* Add an `IsString CharSet` instance.

0.3.7.1
-------
* Minor haddock fixup.

0.3.7
-----
* Switched to derived Typeable for GHC 7.8 compatibility

0.3.6
-----
* Removed some duplicated blocks in `Data.CharSet.Unicode.Block.blocks`, see issue #3.

0.3.5.1
-------
* Updated dependencies to support GHC 7.8

0.3.5
-----
* Claim to be Trustworthy.

0.3.3
-----
* Removed upper bounds on my other packages

