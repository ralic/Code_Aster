subroutine xlmail(fiss, nmaen1, nmaen2, nmaen3, nmafon,&
                  jmaen1, jmaen2, jmaen3, jmafon, nfon,&
                  jfon, nbfond, jbas, jtail, jfonmu,&
                  ndim, goinop)
!
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
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=8) :: fiss
    integer :: nmaen1, nmaen2, nmaen3, nmafon
    integer :: jmaen1, jmaen2, jmaen3, jmafon
    integer :: nfon
    integer :: jfon, jbas, jtail, jfonmu
    integer :: nbfond, ndim
    aster_logical :: goinop
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (CREATION DES SD)
!
! CREATION SD SUR MAILLAGE XFEM
!
! ----------------------------------------------------------------------
!
!
! IN  FISS   : NOM DE LA FISSURE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NMAEN1 : NOMBRE DE MAILLES 'HEAVISIDE'
! IN  NMAEN2 : NOMBRE DE MAILLES 'CRACKTIP'
! IN  NMAEN3 : NOMBRE DE MAILLES 'HEAVISIDE-CRACKTIP'
! IN  NMAFON : NOMBRE DE MAILLES CONTENANT LE FOND DE FISSURE
! IN  JMAEN1 : POINTEUR SUR MAILLES 'HEAVISIDE'
! IN  JMAEN2 : POINTEUR SUR MAILLES 'CRACKTIP'
! IN  JMAEN3 : POINTEUR SUR MAILLES 'HEAVISIDE-CRACKTIP'
! IN  JMAFON : POINTEUR SUR MAILLES CONTENANT LE FOND DE FISSURE
! IN  NFON   : NOMBRE DE POINTS DE FOND DE FISSURE
! IN  JFON   : POINTEUR SUR POINTS DE FOND DE FISSURE
! IN  JBAS   : POINTEUR SUR DIRECTION DE PROPAGATION
! IN  JTAIL  : POINTEUR SUR TAILLES MAXIMALES DE MAILLES
! IN  NBFOND : NOMBRE DE FONDS DE FISSURES DETECTES
! IN  JFONMU : POINTEUR SUR DEBUT ET ARRIVEE DU FOND DE FISSURE
! IN  GOINOP : .TRUE. SI PASSAGE DANS OPOO10 (UPWIND-SIMPLEXE/GRILLE/3D)
!              .FALSE. SINON
!
    character(len=24) :: xheav, xctip, xhect, xmafon, xfonfi, xbasfo, xfonmu
    character(len=24) :: xtailr
    character(len=24) :: xfonfg
!
    integer :: jma1, jma2, jma3, jma4, jfo, jfomu, jba, jta
    integer :: i, k
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES AUX OBJETS
!
    if (.not.goinop) then
        xheav = fiss(1:8)//'.MAILFISS.HEAV'
        xctip = fiss(1:8)//'.MAILFISS.CTIP'
        xhect = fiss(1:8)//'.MAILFISS.HECT'
        xmafon = fiss(1:8)//'.MAILFISS.MAFOND'
        xfonfi = fiss(1:8)//'.FONDFISS'
        xbasfo = fiss(1:8)//'.BASEFOND'
        xtailr = fiss(1:8)//'.FOND.TAILLE_R'
        xfonmu = fiss(1:8)//'.FONDMULT'
!
! --- ENREGISTREMENT DES GROUP_MA 'HEAVISIDE'
!
        if (nmaen1 .ne. 0) then
            call wkvect(xheav, 'G V I', nmaen1, jma1)
            do 810 i = 1, nmaen1
                zi(jma1-1+i) = zi(jmaen1-1+i)
810         continue
        endif
!
! --- ENREGISTREMENT DES GROUP_MA 'CRACKTIP'
!
        if (nmaen2 .ne. 0) then
            call wkvect(xctip, 'G V I', nmaen2, jma2)
            do 820 i = 1, nmaen2
                zi(jma2-1+i) = zi(jmaen2-1+i)
820         continue
        endif
!
! --- ENREGISTREMENT DES GROUP_MA ''HEAVISIDE-CRACKTIP'
!
        if (nmaen3 .ne. 0) then
            call wkvect(xhect, 'G V I', nmaen3, jma3)
            do 830 i = 1, nmaen3
                zi(jma3-1+i) = zi(jmaen3-1+i)
830         continue
        endif
!
! --- ENREGISTREMENT DES MAILLES CONTENANT LE FOND DE FISSURE
!
        if (nmafon .ne. 0) then
            call wkvect(xmafon, 'G V I', nmafon, jma4)
            do 840 i = 1, nmafon
                zi(jma4-1+i) = zi(jmafon-1+i)
840         continue
        endif
!
! --- ENREGISTREMENT DES COORD ET DES ABS CURV
!
        if (nfon .ne. 0) then
            call wkvect(xfonfi, 'G V R', 4*nfon, jfo)
            do 860 i = 1, nfon
                do 850 k = 1, 4
                    zr(jfo-1+4*(i-1)+k) = zr(jfon-1+4*(i-1)+k)
850             continue
860         continue
!
            call wkvect(xbasfo, 'G V R', 2*ndim*nfon, jba)
            do 940 i = 1, nfon
                do 950 k = 1, ndim
                    zr(jba-1+2*ndim*(i-1)+k) = zr(jbas-1+2*ndim*(i-1)+ k)
                    zr(jba-1+2*ndim*(i-1)+k+ndim) = zr( jbas-1+2*ndim*(i-1)+k+ndim)
950             continue
940         continue
!
            call wkvect(xtailr, 'G V R', nfon, jta)
            do 960 i = 1, nfon
                zr(jta-1+i) = zr(jtail-1+i)
960         continue
!
        endif
!
! --- ENREGISTREMENT DES FONDS MULTIPLES
!
        if (nbfond .ne. 0) then
            call wkvect(xfonmu, 'G V I', 2*nbfond, jfomu)
            do 870 i = 1, nbfond
                zi(jfomu-1+2*(i-1)+1) = zi(jfonmu-1+2*(i-1)+1)
                zi(jfomu-1+2*(i-1)+2) = zi(jfonmu-1+2*(i-1)+2)
870         continue
        endif
!
!
    else if (goinop) then
!     CREATION DU FOND DE FISSURE SUR LA GRILLE FONDFISG
!     ET DES POINTS VIRTUELS FONDFISV
!     (UNIQUEMENT SI PROPAGATION UPWIND/SIMPLEXE AVEC GRILLE EN 3D)
        xfonfg = fiss(1:8)//'.FONDFISG'
!
! --- ENREGISTREMENT DES COORD SUR LA GRILLE
!
        call wkvect(xfonfg, 'G V R', 4*nfon, jfo)
        do 880 i = 1, nfon
            do 890 k = 1, 4
                zr(jfo-1+4*(i-1)+k) = zr(jfon-1+4*(i-1)+k)
890         continue
880     continue
    endif
!
    call jedema()
end subroutine
