subroutine irgmpv(ifi, lresu, nomcon, chamsy, nbordr,&
                  para, nocmp, nbel, scal, vect,&
                  tens, versio)
    implicit none
!
    include 'jeveux.h'
!
    include 'asterfort/jenonu.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/lxlgut.h'
    integer :: ifi, nbordr, lch, ich, versio
    real(kind=8) :: para(*)
    logical :: lresu, scal, vect, tens
    character(len=8) :: nocmp
    character(len=*) :: nomcon, chamsy
!     NBRE POUR CHAQUE TYPE D'ELEMENT
    integer :: nbel(*)
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     BUT :   ECRITURE D'UN RESULTAT AU FORMAT GMSH
!
!     ------------------------------------------------------------------
    integer :: ior
    character(len=8) :: nomsd
    character(len=50) :: k50b
    integer :: nbpoi, nbseg, nbtri, nbtet, nbqua, nbpyr, nbpri, nbhex
    integer :: typpoi, typseg, typtri, typtet, typqua
    integer :: typpyr, typpri, typhex
!     ------------------------------------------------------------------
!
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1' ), typpoi)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2' ), typseg)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TRIA3' ), typtri)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'QUAD4' ), typqua)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TETRA4' ), typtet)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PYRAM5' ), typpyr)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PENTA6' ), typpri)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'HEXA8' ), typhex)
    nbpoi=nbel(typpoi)
    nbseg=nbel(typseg)
    nbtri=nbel(typtri)
    nbqua=nbel(typqua)
    nbtet=nbel(typtet)
    nbpyr=nbel(typpyr)
    nbpri=nbel(typpri)
    nbhex=nbel(typhex)
!
    write(ifi,1000) '$View'
!
!     ECRITURE DE LIGNE 1 (VIEW_NAME NB_TIME_STEPS)
!
    nomsd = nomcon(1:8)
!
    if (lresu) then
!
        lch = lxlgut(nomsd)
        k50b(1:lch) = nomsd(1:lch)
        ich = lch+1
!
        k50b(ich:ich) = '_'
        lch=lxlgut(chamsy)
        k50b(ich+1:ich+lch) = chamsy(1:lch)
        ich = ich+lch+1
!
        k50b(ich:ich) = '_'
        lch=lxlgut(nocmp)
        k50b(ich+1:ich+lch) = nocmp(1:lch)
!
        k50b(ich+lch+1:ich+lch+1) = ' '
        write(ifi,1020) k50b(1:ich+lch+1), nbordr
!
    else
!
        lch = lxlgut(nomsd)
        k50b(1:lch) = nomsd(1:lch)
!
        ich = lch+1
        k50b(ich:ich) = '_'
        lch=lxlgut(nocmp)
        k50b(ich+1:ich+lch) = nocmp(1:lch)
!
        k50b(ich+lch+1:ich+lch+1) = ' '
        write(ifi,1022) k50b(1:ich+lch+1), nbordr
!
    endif
!
!     ECRITURE DE LA LIGNE 2 A 4 (nb elt par de type de maille)
!
    if (scal) then
        write(ifi,1030) nbpoi, 0, 0
        write(ifi,1030) nbseg, 0, 0
        write(ifi,1030) nbtri, 0, 0
        if (versio .eq. 2) then
            write(ifi,1030) nbqua, 0, 0
        endif
        write(ifi,1030) nbtet, 0, 0
        if (versio .eq. 2) then
            write(ifi,1030) nbhex, 0, 0
            write(ifi,1030) nbpri, 0, 0
            write(ifi,1030) nbpyr, 0, 0
        endif
    else if (vect) then
        write(ifi,1030) 0, nbpoi, 0
        write(ifi,1030) 0, nbseg, 0
        write(ifi,1030) 0, nbtri, 0
        if (versio .eq. 2) then
            write(ifi,1030) 0, nbqua, 0
        endif
        write(ifi,1030) 0, nbtet, 0
        if (versio .eq. 2) then
            write(ifi,1030) 0, nbhex, 0
            write(ifi,1030) 0, nbpri, 0
            write(ifi,1030) 0, nbpyr, 0
        endif
    else if (tens) then
        write(ifi,1030) 0, 0, nbpoi
        write(ifi,1030) 0, 0, nbseg
        write(ifi,1030) 0, 0, nbtri
        if (versio .eq. 2) then
            write(ifi,1030) 0, 0, nbqua
        endif
        write(ifi,1030) 0, 0, nbtet
        if (versio .eq. 2) then
            write(ifi,1030) 0, 0, nbhex
            write(ifi,1030) 0, 0, nbpri
            write(ifi,1030) 0, 0, nbpyr
        endif
    else
    endif
!
    if (versio .eq. 2) then
        write(ifi,1050) 0,0,0,0
    endif
!
!     ECRITURE DE LA LIGNE 5 (time_step_values)
!
    write(ifi,1040) (para(ior), ior=1,nbordr)
!
    1000 format(a5)
    1020 format(a,1x,i4)
    1022 format(a,1x,i4)
    1030 format(3(i8,1x))
    1040 format(1p,10(e15.8,1x))
    1050 format(4(i6,1x))
!
end subroutine
