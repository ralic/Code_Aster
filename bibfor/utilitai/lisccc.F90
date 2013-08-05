subroutine lisccc(nomcmd, motclc, nbauth, nbnaut, mclaut)
!
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
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/iscode.h"
#include "asterfort/isdeco.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lisdef.h"
    character(len=16) :: nomcmd
    integer :: motclc(2)
    integer :: nbnaut, nbauth, mclaut(2)
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! VERIFICATION COMPATIBILITE CHARGE/COMMANDE - SOUS
!
! ----------------------------------------------------------------------
!
!
! IN  NOMCMD : NOM DE LA COMMANDE
! IN  MOTCLC : CODE (ENTIER CODE) CONTENANT LES MOTS-CLEFS
! OUT NBAUTH : NOMBRE DE MOTS-CLEFS AUTORISES DANS CETTE COMMANDE
! OUT NBNAUT : NOMBRE DE MOTS-CLEFS NON AUTORISES DANS CETTE COMMANDE
! OUT MCLAUT : CODE (ENTIER CODE) CONTENANT LES MOTS-CLEFS AUTORISES
!              DANS CETTE COMMANDE
!
! ----------------------------------------------------------------------
!
    integer :: zbgdlh, zbgccg
    parameter    (zbgdlh = 17,zbgccg = 9)
    character(len=16) :: autdlh(zbgdlh), autccg(zbgccg)
!
    integer :: tabcox(60), tabaut(60)
    integer :: nbtota, nbgcmd
    integer :: ipose, iposit(2), iauth, ibid
    character(len=8) :: k8bid
    character(len=16) :: motclf
    logical :: lfind
!
! --- DYNA_LINE_HARM
!
    data autdlh /&
     &     'DIRI_DUAL'       ,'FORCE_NODALE'    ,'EPSI_INIT'       ,&
     &     'PRES_REP'        ,'FLUX_THM_REP'    ,'PESANTEUR'       ,&
     &     'ROTATION'        ,'FORCE_CONTOUR'   ,'FORCE_INTERNE#3D',&
     &     'FORCE_INTERNE#2D','FORCE_ARETE'     ,'FORCE_FACE'      ,&
     &     'FORCE_POUTRE'    ,'FORCE_COQUE#3D'  ,'FORCE_COQUE#2D'  ,&
     &     'VECT_ASSE'       ,'VECT_ASSE_GENE'  /
!
! --- CALC_G
!
    data autccg /&
     &     'DIRI_DUAL'       ,'EPSI_INIT'       ,'PRES_REP'        ,&
     &     'PESANTEUR'       ,'ROTATION'        ,'FORCE_CONTOUR'   ,&
     &     'FORCE_INTERNE#3D','FORCE_INTERNE#2D','FORCE_FACE'      /
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nbnaut = 0
    nbauth = 0
    nbtota = 0
    do 1 ipose = 1, 60
        tabaut(ipose) = 0
 1  continue
!
! --- DECODAGE DU CHARGEMENT
!
    call isdeco(motclc, tabcox, 60)
!
! --- SELECTION DATA DE LA COMMANDE ACTIVE
!
    if (nomcmd .eq. 'DYNA_LINE_HARM') then
        nbgcmd = zbgdlh
    else if (nomcmd.eq.'CALC_G') then
        nbgcmd = zbgccg
    else
        ASSERT(.false.)
    endif
!
! --- BOUCLE SUR LES MOTS-CLEFS ACTIFS DANS LA CHARGE
!
    do 20 ipose = 1, 60
        if (tabcox(ipose) .eq. 1) then
            lfind = .false.
            nbtota = nbtota+1
!
! --------- BOUCLE SUR LES MOTS-CLEFS AUTORISES
!
            do 21 iauth = 1, nbgcmd
                if (nomcmd .eq. 'DYNA_LINE_HARM') then
                    motclf = autdlh(iauth)
                    call lisdef('POSM', motclf, ibid, k8bid, iposit)
                else if (nomcmd.eq.'CALC_G') then
                    motclf = autccg(iauth)
                    call lisdef('POSM', motclf, ibid, k8bid, iposit)
                else
                    ASSERT(.false.)
                endif
                ASSERT(iposit(1).ne.0)
                if (iposit(1) .eq. ipose) lfind = .true.
21          continue
            if (lfind) then
                nbauth = nbauth + 1
                tabaut(ipose) = 1
            endif
        else
            tabaut(ipose) = 0
        endif
20  continue
!
! --- CODAGE MOT-CLEFS AUTORISES
!
    call iscode(tabaut, mclaut, 60)
!
    call jedema()
end subroutine
