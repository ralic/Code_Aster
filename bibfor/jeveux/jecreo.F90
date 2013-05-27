subroutine jecreo(nomlu, listat)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: j-pierre.lefebvre at edf.fr
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
!
! ROUTINE UTILISATEUR DE CREATION D'UN OBJET JEVEUX
!
! IN  NOMLU  : NOM DE L'OBJET JEVEUX
! IN  LISTAT : CHAINE DE CARACTERES CONTENANT LA LISTES DES ATTRIBUTS
!
! ----------------------------------------------------------------------
    implicit none
    include 'jeveux_private.h'
    include 'asterfort/jjanal.h'
    include 'asterfort/jjvern.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: nomlu, listat
!     ==================================================================
!-----------------------------------------------------------------------
    integer :: iv, jcara, jdate, jdocu, jgenr, jhcod, jiadd
    integer :: jiadm, jlong, jlono, jltyp, jluti, jmarq, jorig
    integer :: jrnom, jtype, n
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
!
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
!     ------------------------------------------------------------------
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
!     ------------------------------------------------------------------
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
!     ------------------------------------------------------------------
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
!     ------------------------------------------------------------------
    character(len=1) :: typei, genri
    integer :: nv, icre, iret
    parameter      ( nv = 3 )
    integer :: lval(nv)
    character(len=8) :: cval(nv)
    character(len=32) :: noml32
    character(len=4) :: ifmt
!     ------------------------------------------------------------------
    if (len(nomlu) .gt. 24) then
        call u2mesk('F', 'JEVEUX_84', 1, nomlu)
    endif
    noml32 = nomlu(1:min(24,len(nomlu)))
!
    call jjanal(listat, nv, nv, lval, cval)
    iclas = index ( classe , cval(1)(1:1) )
    if (iclas .eq. 0) then
        call u2mesk('F', 'JEVEUX_68', 1, cval(1))
    endif
!
    icre = 1
    call jjvern(noml32, icre, iret)
!
    if (iret .eq. 2) then
        call u2mesk('F', 'JEVEUX_85', 1, noml32)
    else
        genr(jgenr(iclaos)+idatos) = cval(2)(1:1)
        type(jtype(iclaos)+idatos) = cval(3)(1:1)
        if (cval(3)(1:1) .eq. 'K' .and. lval(3) .eq. 1) then
            call u2mesk('F', 'JEVEUX_86', 1, noml32)
        else
            genri = genr ( jgenr(iclaos) + idatos )
            typei = type ( jtype(iclaos) + idatos )
            if (genri .eq. 'N' .and. typei .ne. 'K') then
                call u2mesk('F', 'JEVEUX_87', 1, noml32)
            endif
            if (typei .eq. 'K') then
                write(ifmt,'(''(I'',I1,'')'')') lval(3) - 1
                read ( cval(3)(2:lval(3)) , ifmt ) iv
                if (iv .le. 0 .or. iv .gt. 512) then
                    call u2mesk('F', 'JEVEUX_88', 1, cval(3))
                endif
                if (genri .eq. 'N') then
                    if (mod ( iv , lois ) .ne. 0) then
                        call u2mesk('F', 'JEVEUX_89', 1, noml32)
                    endif
                    if (iv .gt. 24) then
                        call u2mesk('F', 'JEVEUX_90', 1, noml32)
                    endif
                endif
            else if (typei .eq. 'I') then
                iv = lois
            else if (typei .eq. 'R') then
                iv = lor8
            else if (typei .eq. 'C') then
                iv = loc8
            else if (typei .eq. 'L') then
                iv = lols
            else if (typei .eq. 'S') then
                iv = lor8/2
            else
                call u2mesk('F', 'JEVEUX_91', 1, cval(3))
            endif
            ltyp ( jltyp(iclaos) + idatos ) = iv
        endif
        if (cval(2)(1:1) .eq. 'E') then
            long(jlong(iclaos)+idatos) = 1
            lono(jlono(iclaos)+idatos) = 1
        endif
    endif
!
end subroutine
