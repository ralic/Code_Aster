subroutine afddli(valr, valk, valc, prnm, nddla,&
                  fonree, nomn, ino, ddlimp, valimr,&
                  valimf, valimc, motcle, direct, dimens,&
                  mod, lisrel, nomcmp, nbcmp, icompt,&
                  lxfem, jnoxfl, jnoxfv, ch1, ch2,&
                  ch3, cnxinv)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRP_21
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterc/indik8.h'
    include 'asterfort/afrela.h'
    include 'asterfort/assert.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/xddlim.h'
    integer :: prnm(*), nddla, ino, ddlimp(nddla), dimens
    integer :: nbcmp, icompt(nddla)
    real(kind=8) :: valr(*), valimr(nddla), direct(3)
    complex(kind=8) :: valc(*), valimc(nddla)
    character(len=4) :: fonree
    character(len=8) :: valk(*), nomn, valimf(nddla), nomcmp(*), mod
    character(len=16) :: motcle(nddla)
    character(len=19) :: cnxinv, lisrel
    logical :: lxfem
    integer :: jnoxfl, jnoxfv
    character(len=19) :: ch1, ch2, ch3
!
!  BUT : * EFFECTUER LE BLOCAGE DES DDLS DONNES PAR MOTCLE(*)
!          SUR LE NOEUD NOMN DANS LA LISTE DE RELATIONS LISREL.
!
!          DANS LE CAS DE RELATIONS REDONDANTES LA REGLE DE LA SURCHAGE
!          EST APPLIQUEE ET C'EST LE DERNIER BLOCAGE SUR LE NOEUD
!          QUI EST APPLIQUE .
!
! ARGUMENTS D'ENTREE:
!
!   PRNM   : DESCRIPTEUR GRANDEUR SUR LE NOEUD INO
!   NDDLA  : NOMBRE DE DDLS DANS DDL_IMPO/FACE_IMPO
!   FONREE : AFFE_CHAR_XXXX OU AFFE_CHAR_XXXX_F
!   NOMN   : NOM DU NOEUD INO OU EST EFFECTUE LE BLOCAGE
!   INO    : NUMERO DU NOEUD OU EST EFFECTUE LE BLOCAGE
!   DDLIMP : INDICATEUR DE PRESENCE OU ABSENCE DE BLOCAGE SUR CHAQUE DDL
!   VALIMR : VALEURS DE BLOCAGE SUR CHAQUE DDL (FONREE = 'REEL')
!   VALIMF : VALEURS DE BLOCAGE SUR CHAQUE DDL (FONREE = 'FONC')
!   VALIMC : VALEURS DE BLOCAGE SUR CHAQUE DDL (FONREE = 'COMP')
!   MOTCLE : TABLEAU DES NOMS DES DDLS DANS DDL_IMPO/FACE_IMPO
!   DIRECT : DIRECTION DE LA COMPOSANTE QUE L'ON VEUT CONTRAINDRE
!            N'EST UTILISEE QUE SI DIMENS DIFFERENT DE 0
!   DIMENS :  SI = 0 ON IMPOSE UN DDL SELON UNE DIRECTION DU REPERE
!             GLOBAL
!             SI =2 OU 3 C'EST LA DIMENSION DE LA GEOMETRIE DU
!             PROBLEME ET LA DIRECTION DE LA COMPOSANTE A CONTRAINDRE
!             EST DONNEE PAR DIRECT(3)
!  MOD     :  NOM DE L'OBJET MODELE ASSOCIE AU LIGREL DE CHARGE
!
! ARGUMENTS D'ENTREE MODIFIES:
!
!      VALR  : VALEURS DE BLOCAGE DES DDLS (FONREE = 'REEL')
!      VALK  : VALEURS DE BLOCAGE DES DDLS (FONREE = 'FONC')
!      VALC  : VALEURS DE BLOCAGE DES DDLS (FONREE = 'COMP')
!     LISREL : LISTE DE RELATIONS AFFECTEE PAR LA ROUTINE
!     ICOMPT(*) : "COMPTEUR" DES DDLS AFFECTES REELLEMENT
!
!
!
    integer :: j, ibid, icmp, iityp
    character(len=16) :: oper
    character(len=8) :: k8b
    character(len=4) :: fonre1, fonre2, typcoe
    character(len=2) :: typlag
    real(kind=8) :: coef, rbid(3)
    complex(kind=8) :: cun
!
!-----------------------------------------------------------------------
!
    call jemarq()
    call getres(k8b, k8b, oper)
    iityp = 0
    if (oper .eq. 'AFFE_CHAR_MECA_C') iityp = 1
    typlag = '12'
    typcoe = 'REEL'
    if (fonree .eq. 'COMP') typcoe = 'COMP'
    if (iityp .eq. 1) typcoe = 'COMP'
!
! --- AFFECTATION DU BLOCAGE A LA LISTE DE RELATIONS LISREL :
!     -----------------------------------------------------
    do 30 j = 1, nddla
!
!       -- SI LA CMP N'EXISTE PAS SUR LE NOEUD, ON SAUTE :
        icmp = indik8(nomcmp,motcle(j)(1:8),1,nbcmp)
        call assert(icmp.gt.0)
        if (.not.exisdg(prnm,icmp)) goto 30
!
        if (lxfem) then
            if (zl(jnoxfl-1+2*ino) .and. motcle(j)(1:1) .eq. 'D') then
                call xddlim(mod, motcle(j)(1:8), nomn, ino, valimr(j),&
                            valimc(j), valimf(j), fonree, icompt(j), lisrel,&
                            ibid, rbid, jnoxfv, ch1, ch2,&
                            ch3, cnxinv)
                goto 30
            endif
        endif
!
!       -- SI LA CMP N'EXISTE PAS SUR LE NOEUD, ON SAUTE :
        icmp = indik8(nomcmp,motcle(j)(1:8),1,nbcmp)
        call assert(icmp.gt.0)
        if (.not.exisdg(prnm,icmp)) goto 30
!
        icompt(j) = icompt(j) + 1
!
        if (ddlimp(j) .ne. 0) then
!
! ---   CAS D'UN BLOCAGE D'UNE GRANDEUR COMPLEXE :
!       ----------------------------------------
            if (iityp .eq. 1) then
                fonre1 = 'REEL'
                fonre2 = 'COMP'
                coef = 1.0d0
                cun = dcmplx(1.0d0,0.0d0)
                call afrela(coef, cun, motcle(j) (1:8), nomn, dimens,&
                            direct, 1, valimr(j), valimc(j), valimf(j),&
                            fonre1, fonre2, typlag, 0.d0, lisrel)
!
! ---   CAS D'UN BLOCAGE D'UNE GRANDEUR REELLE :
!       --------------------------------------
            else
                coef = 1.0d0
                cun = dcmplx(1.0d0,0.0d0)
                call afrela(coef, cun, motcle(j) (1:8), nomn, dimens,&
                            direct, 1, valimr(j), valimc(j), valimf(j),&
                            typcoe, fonree, typlag, 0.d0, lisrel)
            endif
!
            if (fonree .eq. 'REEL') then
                valr(nddla* (ino-1)+j) = valimr(j)
            else if (fonree.eq.'COMP') then
                valc(nddla* (ino-1)+j) = valimc(j)
            else
                valk(nddla* (ino-1)+j) = valimf(j)
            endif
        endif
30  end do
!
    call jedema()
end subroutine
