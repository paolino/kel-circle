# Changelog

## 1.0.0 (2026-03-01)


### Features

* add docker image build+push CI workflow ([c668d8b](https://github.com/paolino/kel-circle/commit/c668d8b2c4ea3c3a163e9d01e41523410ca6cc3c))
* add kel-circle-server executable with static file serving ([d0a69ed](https://github.com/paolino/kel-circle/commit/d0a69edf7857fd88177cd0d9e2c314b0bee337e5))
* add server-side logging for event submissions ([8fb0119](https://github.com/paolino/kel-circle/commit/8fb0119b0cae2a1221b41ef7d665989cf1fd1c98))
* admin majority, demotion re-enters bootstrap ([02a1843](https://github.com/paolino/kel-circle/commit/02a184362cfd9eb1c639e3938f0df55c555d5c86))
* application gate participates in member removal gating ([16a767b](https://github.com/paolino/kel-circle/commit/16a767b6bac6b90f9f149f330f2603c19e74cd6b))
* base decisions with introduce-member and admin gating ([a60d63b](https://github.com/paolino/kel-circle/commit/a60d63b9245b9ad99c86c874a94af80d348ef103))
* cache KelKeyState in FullState to eliminate SQLite reads ([440db0d](https://github.com/paolino/kel-circle/commit/440db0d2d06e4a9cbda98532ca99682bdabf9098)), closes [#25](https://github.com/paolino/kel-circle/issues/25)
* CESR validation for MemberId (Phase 2, Step 2.1) ([0d714d4](https://github.com/paolino/kel-circle/commit/0d714d4c07d8f6daeb23a44712b825ca06e808ab))
* enforce unique member name invariant ([17d796b](https://github.com/paolino/kel-circle/commit/17d796b57271a16081b9c933921793bb9a1f16ae))
* event processing pipeline with preservation proofs ([78ee5b2](https://github.com/paolino/kel-circle/commit/78ee5b223d61f61a49b4005b52e55bd11d1b7f75))
* fix SSE race condition, broadcast all KEL updates, add Playwright e2e tests ([259a281](https://github.com/paolino/kel-circle/commit/259a281ee1401aac2bff65182c991a5d7bb84078))
* formalize kel-circle invariants in Lean 4 ([f16ee37](https://github.com/paolino/kel-circle/commit/f16ee3733ded7082c0a6ed1fa0df6398ae474561))
* formalize proposal lifecycle in Lean ([9f737dd](https://github.com/paolino/kel-circle/commit/9f737dd1781936fb8b6156f11c7e846d3e44cf04))
* genesis event and sequencer protection invariants ([75f3451](https://github.com/paolino/kel-circle/commit/75f3451373ad9dc1efba24a23696f883f78f4ba1))
* give sequencer a real KERI AID with server-side signing ([9bb2ade](https://github.com/paolino/kel-circle/commit/9bb2ade7aebcf7a40492da4143453d81fe89090e))
* Haskell library skeleton mirroring Lean formalization ([2a00370](https://github.com/paolino/kel-circle/commit/2a0037069e29dc92b11d9dda6d4ef7acbb0f79dd))
* HTTP server, SQLite store, and 15 E2E tests ([e1de3ee](https://github.com/paolino/kel-circle/commit/e1de3ee34dc4d01855e56868e1d83ae6473e8370))
* identity export/import with E2E tests ([1f96b71](https://github.com/paolino/kel-circle/commit/1f96b7176e61b056ac6a104ccb3d0a652f04deaa))
* initial repository skeleton ([014c076](https://github.com/paolino/kel-circle/commit/014c07627e0bff4b5d74278c643b9089b7bffb16))
* KERI-backed client crypto with encrypted identity persistence ([2ae672b](https://github.com/paolino/kel-circle/commit/2ae672b4c21951c0af31194eb7a0035720bcc9ad))
* key rotation support with pre-rotation commitment verification (Phase 2, Step 2.4) ([0349051](https://github.com/paolino/kel-circle/commit/0349051c53039acc4c56918b0a99eef854db4ea3))
* per-member KEL storage and inception validation (Phase 2, Step 2.2) ([3c30e9a](https://github.com/paolino/kel-circle/commit/3c30e9a1778ec701d2559e3421d522e5bd5e4764))
* PureScript client skeleton with two-package workspace ([2b2d28e](https://github.com/paolino/kel-circle/commit/2b2d28e92648e45919f836baab4273aa8be135d5))
* sequencer rotation with admin majority ([0b597cd](https://github.com/paolino/kel-circle/commit/0b597cd595e16318509c73432a7c9f47e4e6c78a))
* validation module and 53 QuickCheck tests mirroring Lean theorems ([e4b6dbf](https://github.com/paolino/kel-circle/commit/e4b6dbff7a790aa191340fb63ea1c16bcbba9aa8))
* verify circle events as KERI interaction events (Phase 2, Step 2.3) ([03999d1](https://github.com/paolino/kel-circle/commit/03999d1ebe86059984833f94158ba7d0ae81e256))


### Bug Fixes

* add keyfile arg to Docker entrypoint ([c02d2c7](https://github.com/paolino/kel-circle/commit/c02d2c77a9a00a9a7590a76b8559f51990439b77))
* add missing scLog to test ServerConfig ([fb62018](https://github.com/paolino/kel-circle/commit/fb620186ed3775839bbf475fe611b7cb8e10e74d))
* catch decrypt error on unlock and add reset button ([28f598b](https://github.com/paolino/kel-circle/commit/28f598b37728c269a51102ed733684acb37abfc6))
* CI runs only Lean proofs and docs in design phase ([145b27f](https://github.com/paolino/kel-circle/commit/145b27f1a8e63eca5fbd758187a833b853ba23a4))
* correct event polling offset, gate target checks, and client sequencer id ([274e5e2](https://github.com/paolino/kel-circle/commit/274e5e2dfd5e908219644cd1be5209bf32cea6ce))
* decode stored event JSON as Value in GET /events ([600508a](https://github.com/paolino/kel-circle/commit/600508ac0a46bddad97ec42ba0d96a88b59a84bf))
* enforce admin-only responses and reject duplicate proposals ([ed74b16](https://github.com/paolino/kel-circle/commit/ed74b16d8f2b236a842d26e1fdc5074c1310fc07))
* handle git dependencies in nix client bundle ([b5585ee](https://github.com/paolino/kel-circle/commit/b5585ee1f074d8b94273aae63ab7af57532f8119))
* mermaid parse errors in architecture doc ([d8b8040](https://github.com/paolino/kel-circle/commit/d8b8040db55006b651162495a80115d8224b5829))
* remove conflict marker in Gate.hs, add challenge-response to roadmap ([92981ca](https://github.com/paolino/kel-circle/commit/92981ca3b94f3bcfb9ffe270c06992ac65366c07))
* render mermaid diagrams and clean up trust boundary layout ([64ef13f](https://github.com/paolino/kel-circle/commit/64ef13f3f48a11f009e03f094a585c09ad42a85e))
* replace \n with &lt;br/&gt; in mermaid diagram labels ([027b42d](https://github.com/paolino/kel-circle/commit/027b42d41bb95d4b8c4f44ace9f4441ae9e55107))
* show NonMemberScreen when user key is not in circle ([882eaaf](https://github.com/paolino/kel-circle/commit/882eaaf92640ab4e16313029bb5c6b01c156b2c6))
* sort JSON keys alphabetically in PureScript codecs ([ce8ed1d](https://github.com/paolino/kel-circle/commit/ce8ed1d5f9a032f824230dc6f3a662cef5405bd9))
* straight gate rejects admin role grants, requires majority ([6ae9012](https://github.com/paolino/kel-circle/commit/6ae901223ad2707a49c4b8e32c4a08db221ad75b))
* use Number for Timestamp to handle millisecond epoch values ([7378064](https://github.com/paolino/kel-circle/commit/737806470c5a45f7dc7fdc5e315c4048d4532947))
