subroutine jeexin(nomlu, iret)
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
    implicit none
    include 'jeveux_private.h'
    include 'asterfort/jjallc.h'
    include 'asterfort/jjcroc.h'
    include 'asterfort/jjvern.h'
    include 'asterfort/jxveuo.h'
    character(len=*) :: nomlu
    integer :: iret
! ----------------------------------------------------------------------
! ROUTINE UTILISATEUR : TESTE L'EXISTENCE DU DESCRIPTEUR CREE PAR
!                       JECREO OU JECROC
! IN  NOMLU  : NOM DE L'OBJET JEVEUX
! OUT IRET   : =0 LE DESCRIPTEUR N'EXISTE PAS
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ibacol, iblong, ibnum, ic, icre, id, ilong
    integer :: itab, ixlong, ixnom, ixnum, jcara, jctab, jdate
    integer :: jhcod, jiadd, jiadm, jlong, jlono, jltyp, jluti
    integer :: jmarq, n, nuti
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
! ----------------------------------------------------------------------
    integer :: idnom, idlong, idnum
    parameter    ( idnom  = 5 , idlong = 7 ,idnum  = 10 )
! ----------------------------------------------------------------------
    character(len=32) :: noml32
! DEB ------------------------------------------------------------------
    noml32 = nomlu
    iret = 0
    icre = 0
    id = 0
    call jjvern(noml32, icre, iret)
    if (iret .eq. 1) then
        if (noml32(25:32) .eq. '        ') then
            id = idatos
        else
            if (iadm(jiadm(iclaos)+2*idatos-1) .eq. 0) then
                call jxveuo('L', itab, iret, jctab)
            endif
            call jjcroc('        ', icre)
            id = idatoc
        endif
    else if (iret .eq. 2) then
        ic = iclaco
        if (noml32(25:32) .eq. '        ') then
            id = idatco
        else
            call jjallc(ic, idatco, 'L', ibacol)
            call jjcroc(noml32(25:32), icre)
            ixnum = iszon(jiszon+ibacol+idnum )
            ixnom = iszon(jiszon+ibacol+idnom )
            ixlong = iszon(jiszon+ibacol+idlong)
            if (ixnum .ne. 0) then
                ibnum = iadm(jiadm(ic)+2*ixnum-1)
                nuti = iszon(jiszon+ibnum+1)
            else
                nuti = luti ( jluti(ic) + ixnom )
            endif
            id = idatoc
            if (ixlong .ne. 0) then
                iblong = iadm(jiadm(ic)+2*ixlong-1)
                ilong = iszon(jiszon+iblong - 1 + idatoc)
                if (ilong .le. 0) id = 0
            else
                if (idatoc .gt. nuti) id = 0
            endif
        endif
    endif
    iret = id
! FIN ------------------------------------------------------------------
end subroutine
