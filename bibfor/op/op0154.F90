subroutine op0154()
    implicit none
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   OPERATEUR: MODI_MAILLAGE
!
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/abscur.h"
#include "asterfort/asccou.h"
#include "asterfort/asceli.h"
#include "asterfort/ascrep.h"
#include "asterfort/asctub.h"
#include "asterfort/cargeo.h"
#include "asterfort/chgref.h"
#include "asterfort/chpver.h"
#include "asterfort/conori.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/echell.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/momaba.h"
#include "asterfort/orilgm.h"
#include "asterfort/orishb.h"
#include "asterfort/piqeli.h"
#include "asterfort/piqini.h"
#include "asterfort/rotama.h"
#include "asterfort/symema.h"
#include "asterfort/tranma.h"
#include "asterfort/utmess.h"
#include "asterfort/vdiff.h"
#include "asterfort/vecini.h"
#include "asterfort/vtgpld.h"
    integer :: n1, n2, nbocc, nboc1, nboc2, nop, i, dim, ier
    logical(kind=1) :: bidim
    character(len=8) :: ma, ma2, depla, coutur, mab
    character(len=16) :: kbi1, kbi2, option
    character(len=19) :: geomi, geomf
    character(len=24) :: valk(3)
    real(kind=8) :: ltchar, pt(3), pt2(3), dir(3), angl
!
    real(kind=8) :: axe1(3), axe2(3), perp(3)
!
! -DEB------------------------------------------------------------------
!
    call infmaj()
!
    call getvid(' ', 'MAILLAGE', scal=ma, nbret=n1)
!
    call getres(ma2, kbi1, kbi2)
!
    if (ma .ne. ma2) then
        call utmess('F', 'SOUSTRUC_15')
    endif
!
!
!     --- TRAITEMENT DU MOT CLEF  "ORIE_FISSURE" :
!     ---------------------------------------------
    call getfac('ORIE_FISSURE', nbocc)
    if (nbocc .ne. 0) then
        call conori(ma)
    endif
!
!
!     --- TRAITEMENT DU MOT CLEF  "MODI_MAILLE" :
!     ---------------------------------------------
    call getfac('MODI_MAILLE', nbocc)
    if (nbocc .ne. 0) then
        call getvtx('MODI_MAILLE', 'OPTION', iocc=1, scal=option, nbret=n1)
        if (option .eq. 'NOEUD_QUART') then
            call momaba(ma)
        endif
    endif
!
!
!     --- TRAITEMENT DU MOT CLEF  "DEFORME" :
!     ---------------------------------------
    call getfac('DEFORME', nbocc)
    if (nbocc .ne. 0) then
        call getvtx('DEFORME', 'OPTION', iocc=1, scal=option, nbret=nop)
        call getvid('DEFORME', 'DEPL', iocc=1, scal=depla, nbret=n1)
        call dismoi('NOM_MAILLA', depla, 'CHAM_NO', repk=mab)
        if (mab .ne. ma) then
            valk(1)=ma
            valk(2)=mab
            call utmess('F', 'CALCULEL5_1', nk=2, valk=valk)
        endif
        call chpver('F', depla, 'NOEU', 'DEPL_R', ier)
        geomi = ma//'.COORDO'
        geomf = ma//'.COORD2'
        call vtgpld('CUMU', geomi, 1.d0, depla, 'V',&
                    geomf)
        call detrsd('CHAMP_GD', geomi)
        call copisd('CHAMP_GD', 'G', geomf, geomi)
        call detrsd('CHAMP_GD', geomf)
    endif
!
!
!     --- TRAITEMENT DU MOT CLEF  "TRANSLATION" :
!     ---------------------------------------
    call getvr8(' ', 'TRANSLATION', nbval=0, nbret=n1)
    if (n1 .ne. 0) then
        geomi = ma//'.COORDO'
        bidim = .false.
        call getvr8(' ', 'TRANSLATION', nbval=0, nbret=dim)
        dim = - dim
        if (dim .eq. 2) then
            call getvr8(' ', 'TRANSLATION', nbval=2, vect=dir, nbret=n1)
            dir(3) = 0.d0
            bidim = .true.
        else
            call getvr8(' ', 'TRANSLATION', nbval=3, vect=dir, nbret=n1)
        endif
        call tranma(geomi, dir, bidim)
    endif
!
!
!     --- TRAITEMENT DU MOT CLEF  "MODI_BASE" :
!     ---------------------------------------
    call getfac('MODI_BASE', nbocc)
    if (nbocc .ne. 0) then
        geomi = ma//'.COORDO'
        bidim = .false.
        call getvr8('MODI_BASE', 'VECT_X', iocc=1, nbval=0, nbret=dim)
        dim = - dim
        if (dim .eq. 2) then
            call getvr8('MODI_BASE', 'VECT_X', iocc=1, nbval=2, vect=pt,&
                        nbret=n1)
            pt(3) = 0.d0
            call vecini(3, 0.d0, pt2)
            bidim = .true.
        else
            call getvr8('MODI_BASE', 'VECT_X', iocc=1, nbval=3, vect=pt,&
                        nbret=n1)
            call getvr8('MODI_BASE', 'VECT_Y', iocc=1, nbval=3, vect=pt2,&
                        nbret=n1)
        endif
        call chgref(geomi, pt, pt2, bidim)
    endif
!
!
!     --- TRAITEMENT DU MOT CLEF  "ROTATION" :
!     ---------------------------------------
    call getfac('ROTATION', nbocc)
    if (nbocc .ne. 0) then
        geomi = ma//'.COORDO'
        bidim = .false.
        do i = 1, nbocc
            call getvr8('ROTATION', 'POIN_1', iocc=i, nbval=0, nbret=dim)
            call getvr8('ROTATION', 'ANGLE', iocc=i, scal=angl, nbret=n1)
            call getvr8('ROTATION', 'POIN_2', iocc=i, nbval=0, nbret=n2)
            dim = - dim
            if (dim .eq. 2) then
                call getvr8('ROTATION', 'POIN_1', iocc=i, nbval=2, vect=pt,&
                            nbret=n1)
                pt(3) = 0.d0
                call vecini(3, 0.d0, pt2)
                call vecini(3, 0.d0, dir)
                bidim = .true.
            else
                call getvr8('ROTATION', 'POIN_1', iocc=i, nbval=3, vect=pt,&
                            nbret=n1)
                if (n2 .ne. 0) then
                    call getvr8('ROTATION', 'POIN_2', iocc=i, nbval=3, vect=pt2,&
                                nbret=n1)
                    call vdiff(3, pt2, pt, dir)
                else
                    call getvr8('ROTATION', 'DIR', iocc=i, nbval=3, vect=dir,&
                                nbret=n1)
                endif
            endif
            call rotama(geomi, pt, dir, angl, bidim)
        end do
    endif
!
!
!     --- TRAITEMENT DU MOT CLEF  "SYMETRIE" :
!     ---------------------------------------
    call getfac('SYMETRIE', nbocc)
    if (nbocc .ne. 0) then
        geomi = ma//'.COORDO'
        do i = 1, nbocc
            call getvr8('SYMETRIE', 'POINT', iocc=i, nbval=0, nbret=dim)
            call getvr8('SYMETRIE', 'AXE_1', iocc=i, nbval=0, nbret=n1)
            call getvr8('SYMETRIE', 'AXE_2', iocc=i, nbval=0, nbret=n2)
!
!           DIM, N1, N2 = 2 OU 3 ==> IMPOSE PAR LES CATALOGUES
!           EN 2D : DIM=N1=2    , AXE_2 N'EXISTE PAS N2=0
!           EN 3D : DIM=N1=N2=3
            if (dim .eq. -2) then
                if (n1 .ne. dim) then
                    call utmess('F', 'ALGORITH9_62')
                endif
                if (n2 .ne. 0) then
                    call utmess('A', 'ALGORITH9_63')
                endif
                call getvr8('SYMETRIE', 'POINT', iocc=i, nbval=2, vect=pt,&
                            nbret=dim)
                call getvr8('SYMETRIE', 'AXE_1', iocc=i, nbval=2, vect=axe1,&
                            nbret=n1)
!              CONSTRUCTION DU VECTEUR PERPENDICULAIRE A Z ET AXE1
                perp(1) = -axe1(2)
                perp(2) = axe1(1)
                perp(3) = 0.0d0
            else
                if (n1 .ne. dim) then
                    call utmess('F', 'ALGORITH9_62')
                endif
                if (n2 .ne. dim) then
                    call utmess('F', 'ALGORITH9_64')
                endif
                call getvr8('SYMETRIE', 'POINT', iocc=i, nbval=3, vect=pt,&
                            nbret=dim)
                call getvr8('SYMETRIE', 'AXE_1', iocc=i, nbval=3, vect=axe1,&
                            nbret=n1)
                call getvr8('SYMETRIE', 'AXE_2', iocc=i, nbval=3, vect=axe2,&
                            nbret=n2)
!              CONSTRUCTION DU VECTEUR PERPENDICULAIRE A AXE1 ET AXE2
                perp(1) = axe1(2)*axe2(3) - axe1(3)*axe2(2)
                perp(2) = axe1(3)*axe2(1) - axe1(1)*axe2(3)
                perp(3) = axe1(1)*axe2(2) - axe1(2)*axe2(1)
            endif
            call symema(geomi, perp, pt)
        end do
    endif
!
!
!     --- TRAITEMENT DU MOT CLEF  "ECHELLE" :
!     ---------------------------------------
    call getvr8(' ', 'ECHELLE', nbval=0, nbret=n1)
    if (n1 .ne. 0) then
        geomi = ma//'.COORDO'
        call getvr8(' ', 'ECHELLE', scal=ltchar, nbret=n2)
        call echell(geomi, ltchar)
    endif
!
!
!     --- TRAITEMENT DU MOT CLEF  "EQUE_PIQUA" :
!     ------------------------------------------
    call getfac('EQUE_PIQUA', nbocc)
    if (nbocc .ne. 0) then
        call piqini(ma)
        call piqeli(ma)
    endif
!
!
!     --- TRAITEMENT DES MOTS CLES  "ORIE_PEAU_2D" , "ORIE_PEAU_3D"
!                               ET  "ORIE_NORM_COQUE" :
!     ---------------------------------------------------------------
    call orilgm(ma)
!
!     --- TRAITEMENT DU MOT CLEF  "ORIE_SHB" :
!     ------------------------------------------
    call getfac('ORIE_SHB', nbocc)
    if (nbocc .ne. 0) then
        call orishb(ma)
    endif
!
!     --- TRAITEMENT DES MOT CLEF  "PLAQ_TUBE" ET "TUBE_COUDE":
!     --------------------------------------------------------
    call getfac('PLAQ_TUBE', nboc1)
    if (nboc1 .ne. 0) then
        call getvr8('PLAQ_TUBE', 'L_TUBE_P1', iocc=1, scal=ltchar, nbret=n1)
        call getvtx('PLAQ_TUBE', 'COUTURE', iocc=1, scal=coutur, nbret=n1)
        if (coutur .eq. 'OUI') call asceli(ma)
        call asctub(ma)
    endif

    call getfac('TUBE_COUDE', nboc2)
    if (nboc2 .ne. 0) then
        call getvr8('TUBE_COUDE', 'L_TUBE_P1', iocc=1, scal=ltchar, nbret=n1)
        call asccou(ma)
    endif
    if (nboc1 .ne. 0 .or. nboc2 .ne. 0) then
        call ascrep(ma, ltchar)
    endif
!
!
!     --- TRAITEMENT DU MOT CLEF  "ABSC_CURV" :
!     ---------------------------------------
    call getfac('ABSC_CURV', nbocc)
    if (nbocc .eq. 1) then
        call abscur(ma)
    endif
!
!
!     --- on complete le maillage avec quelques objets :
!     ---------------------------------------------------
    call cargeo(ma)
!
!
end subroutine
