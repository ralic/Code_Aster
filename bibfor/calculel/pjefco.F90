subroutine pjefco(moa1, moa2, corres, base)
    implicit none
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!     COMMANDE:  PROJ_CHAMP  METHODE:'ELEM'
! BUT : CALCULER LA STRUCTURE DE DONNEE CORRESP_2_MAILLA
! ----------------------------------------------------------------------
!
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/pj2dco.h"
#include "asterfort/pj3dco.h"
#include "asterfort/pj4dco.h"
#include "asterfort/pj6dco.h"
#include "asterfort/pjefca.h"
#include "asterfort/pjeftg.h"
#include "asterfort/pjfuco.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
    character(len=8) :: moa1, moa2
    character(len=16) :: corres
    character(len=1) :: base
!
! 0.2. ==> COMMUNS
!
!
!
! 0.3. ==> VARIABLES LOCALES
!
!
    character(len=8) :: noma1, noma2, nomo1, nomo2, ncas
    character(len=16) :: corre1, corre2, corre3
    character(len=16) :: tymocl(5), motcle(5)
    character(len=24) :: geom2, geom1
    integer :: n1, nbocc, iocc, nbno2, nbma1
    integer :: iagno2, iagma1, iexi
!
    logical :: ldmax, dbg
    real(kind=8) :: distma
!----------------------------------------------------------------------
    call jemarq()
    ASSERT(base.eq.'V')
!
    corre1 = '&&PJEFCO.CORRES1'
    corre2 = '&&PJEFCO.CORRES2'
    corre3 = '&&PJEFCO.CORRES3'
!
    call jeexin(moa1//'.MODELE    .REPE', iexi)
    if (iexi .gt. 0) then
        nomo1=moa1
        call dismoi('NOM_MAILLA', nomo1, 'MODELE', repk=noma1)
    else
        nomo1=' '
        noma1=moa1
    endif
!
    call jeexin(moa2//'.MODELE    .REPE', iexi)
    if (iexi .gt. 0) then
        nomo2=moa2
        call dismoi('NOM_MAILLA', nomo2, 'MODELE', repk=noma2)
    else
        nomo2=' '
        noma2=moa2
    endif
!
!
!     DETERMINATION DE DISTMA ET LDMAX:
!     --------------------------------------------------------
    ldmax = .false.
    distma = r8maem()
    call getvr8(' ', 'DISTANCE_MAX', scal=distma, nbret=n1)
    if (n1 .eq. 1) ldmax = .true.
!
!
    call getfac('VIS_A_VIS', nbocc)
    if (nbocc .eq. 0) then
!        -- CAS : TOUT:'OUI'
!        ------------------------
        call pjefca(moa1, ' ', 0, ncas)
!
!        PRISE EN COMPTE DU MOT-CLE TRANSF_GEOM_[1|2]
!        --------------------------------------------
        call pjeftg(1, geom1, noma1, ' ', 1)
        call pjeftg(2, geom2, noma2, ' ', 1)
!
!
!
        dbg=.false.
        if (dbg) then
!          -- pour debug : on copie les 2 maillages sous les noms
!          'XXXMA1' et 'XXXMA2' sur la base globale
!          ce qui permet de les visualiser avec Salome.
!          Il faut faire :
!             XXXMA1=LIRE_MAILLAGE() # bidon
!             XXXMA2=LIRE_MAILLAGE() # bidon
!             U2=PROJ_CHAMP(...)
!             IMPR_RRESU(RESU=_F(MAILLAGE=XXXMA1))
!             IMPR_RRESU(RESU=_F(MAILLAGE=XXXMA2))
!
            call detrsd('MAILLAGE', 'XXXMA1')
            call detrsd('MAILLAGE', 'XXXMA2')
            call copisd('MAILLAGE', 'G', noma1, 'XXXMA1')
            call copisd('MAILLAGE', 'G', noma2, 'XXXMA2')
        endif
!
!
        if (ncas .eq. '2D') then
            call pj2dco('TOUT', moa1, moa2, 0, [0],&
                        0, [0], geom1, geom2, corres,&
                        ldmax, distma)
        else if (ncas.eq.'3D') then
            call pj3dco('TOUT', moa1, moa2, 0, [0],&
                        0, [0], geom1, geom2, corres,&
                        ldmax, distma)
        else if (ncas.eq.'2.5D') then
            call pj4dco('TOUT', moa1, moa2, 0, [0],&
                        0, [0], geom1, geom2, corres,&
                        ldmax, distma, ' ')
        else if (ncas.eq.'1.5D') then
            call pj6dco('TOUT', moa1, moa2, 0, [0],&
                        0, [0], geom1, geom2, corres,&
                        ldmax, distma)
        else
            ASSERT(.false.)
        endif
!
    else
!
!        -- CAS : VIS_A_VIS
!        ------------------------
!
!       -- le mot cle VIS_A_VIS ne peut pas fonctionner avec la methode ECLA_PG :
        if (noma1(1:2) .eq. '&&') then
            call utmess('F', 'CALCULEL4_17')
        endif
!
        do 30 iocc = 1, nbocc
!
!           -- RECUPERATION DE LA LISTE DE MAILLES LMA1 :
!           ----------------------------------------------
            motcle(1) = 'MAILLE_1'
            tymocl(1) = 'MAILLE'
            motcle(2) = 'GROUP_MA_1'
            tymocl(2) = 'GROUP_MA'
            motcle(3) = 'TOUT_1'
            tymocl(3) = 'TOUT'
!
!
!
            call reliem(nomo1, noma1, 'NU_MAILLE', 'VIS_A_VIS', iocc,&
                        3, motcle, tymocl, '&&PJEFCO.LIMANU1', nbma1)
            call jeveuo('&&PJEFCO.LIMANU1', 'L', iagma1)
!
!           -- RECUPERATION DE LA LISTE DE NOEUDS LNO2 :
!           ----------------------------------------------
            motcle(1) = 'NOEUD_2'
            tymocl(1) = 'NOEUD'
            motcle(2) = 'GROUP_NO_2'
            tymocl(2) = 'GROUP_NO'
            motcle(3) = 'MAILLE_2'
            tymocl(3) = 'MAILLE'
            motcle(4) = 'GROUP_MA_2'
            tymocl(4) = 'GROUP_MA'
            motcle(5) = 'TOUT_2'
            tymocl(5) = 'TOUT'
            call reliem(' ', noma2, 'NU_NOEUD', 'VIS_A_VIS', iocc,&
                        5, motcle, tymocl, '&&PJEFCO.LINONU2', nbno2)
            call jeveuo('&&PJEFCO.LINONU2', 'L', iagno2)
!
!           PRISE EN COMPTE DU MOT-CLE TRANSF_GEOM_[1|2]
!           --------------------------------------------
            call pjeftg(1, geom1, noma1, 'VIS_A_VIS', iocc)
            call pjeftg(2, geom2, noma2, 'VIS_A_VIS', iocc)
!
!           -- CALCUL DU CORRESP_2_MAILLA POUR IOCC :
!           ----------------------------------------------
            call pjefca(moa1, '&&PJEFCO.LIMANU1', iocc, ncas)
!
            call detrsd('CORRESP_2_MAILLA', corre1)
            if (ncas .eq. '2D') then
                call pj2dco('PARTIE', moa1, moa2, nbma1, zi(iagma1),&
                            nbno2, zi(iagno2), geom1, geom2, corre1,&
                            ldmax, distma)
            else if (ncas.eq.'3D') then
                call pj3dco('PARTIE', moa1, moa2, nbma1, zi(iagma1),&
                            nbno2, zi(iagno2), geom1, geom2, corre1,&
                            ldmax, distma)
            else if (ncas.eq.'2.5D') then
                call pj4dco('PARTIE', moa1, moa2, nbma1, zi(iagma1),&
                            nbno2, zi(iagno2), geom1, geom2, corre1,&
                            ldmax, distma, ' ')
            else if (ncas.eq.'1.5D') then
                call pj6dco('PARTIE', moa1, moa2, nbma1, zi(iagma1),&
                            nbno2, zi(iagno2), geom1, geom2, corre1,&
                            ldmax, distma)
            else
                ASSERT(.false.)
            endif
!
!
!           -- SURCHARGE DU CORRESP_2_MAILLA :
!           ----------------------------------------------
            if (iocc .eq. 1) then
                call copisd('CORRESP_2_MAILLA', 'V', corre1, corre2)
            else
                call pjfuco(corre2, corre1, 'V', corre3)
                call detrsd('CORRESP_2_MAILLA', corre2)
                call copisd('CORRESP_2_MAILLA', 'V', corre3, corre2)
                call detrsd('CORRESP_2_MAILLA', corre3)
            endif
!
            call jedetr('&&PJEFCO.LIMANU1')
            call jedetr('&&PJEFCO.LINONU2')
 30     continue
        call copisd('CORRESP_2_MAILLA', 'V', corre2, corres)
        call detrsd('CORRESP_2_MAILLA', corre1)
        call detrsd('CORRESP_2_MAILLA', corre2)
    endif
!
!
    call jedema()
end subroutine
