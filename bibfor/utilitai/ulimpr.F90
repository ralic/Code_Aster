subroutine ulimpr(impr)
    implicit none
    integer :: impr
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: j-pierre.lefebvre at edf.fr
!
!     IMPRESSION DES TABLES DECRIVANT LES UNITES LOGIQUE OUVERTES
!
    integer :: mxf
    parameter       (mxf=100)
    character(len=1) :: typefi(mxf), accefi(mxf), etatfi(mxf), modifi(mxf)
    character(len=16) :: ddname(mxf)
    character(len=255) :: namefi(mxf)
    integer :: first, unitfi(mxf), nbfile
    common/ asgfi1 / first, unitfi      , nbfile
    common/ asgfi2 / namefi,ddname,typefi,accefi,etatfi,modifi
!
    integer :: i
    character(len=8) :: ktyp, kacc, keta
!
    write(impr,999) 'LA TABLE A CONTENU JUSQU''A ',nbfile,&
     &                ' ASSOCIATION(S)'
    do 1 i = 1, nbfile
        write(impr,1000) namefi(i)
        ktyp='?'
        if (typefi(i) .eq. 'A') then
            ktyp='ASCII'
        else if (typefi(i) .eq. 'B') then
            ktyp='BINARY'
        else if (typefi(i) .eq. 'L') then
            ktyp='LIBRE'
        endif
        kacc='?'
        if (accefi(i) .eq. 'N') then
            kacc='NEW'
        else if (accefi(i) .eq. 'O') then
            kacc='OLD'
        else if (accefi(i) .eq. 'A') then
            kacc='APPEND'
        endif
        keta='?'
        if (etatfi(i) .eq. 'O') then
            keta='OPEN'
        else if (etatfi(i) .eq. 'F') then
            keta='CLOSE'
        else if (etatfi(i) .eq. 'R') then
            keta='RESERVE '
        endif
        write(impr,1001) ddname(i),unitfi(i),ktyp,kacc,keta,modifi(i)
 1  end do
!
    999 format (a,i4,a)
    1000 format (1x,a)
    1001 format (6x,a16,i3,3(1x,a8),1x,a1)
end subroutine
