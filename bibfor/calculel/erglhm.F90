subroutine erglhm(perman, jceld, iavale, iord, ligrel,&
                  longt, nbgr, resuc1)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nbelem.h"
#include "asterfort/rsadpa.h"
    integer :: jceld, iavale, iord, longt, nbgr
    character(len=19) :: ligrel, resuc1
    aster_logical :: perman
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
! =====================================================================
!  ERREUR GLOBALE AU MAILLAGE - HYDRO-MECANIQUE
!  **     **                    *     *
! =====================================================================
!     BUT:
!         CALCUL DES ESTIMATEURS GLOBAUX POUR L'HYDRO-MECANIQUE
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   PERMAN : MODELISATION THM PERMANENTE ?
! IN   JCELD  : ADRESSE DU DESCRIPTEUR DU CHAMP LOCAL
! IN   IAVALE : ADRESSE DES CHAMPS LOCAUX
! IN   IORD   : NUMERO D'ORDRE
! IN   LIGREL : NOM DU LIGREL
! IN   LONGT  : NOMBRE DE CHAMPS LOCAUX
! IN   NBGR   : NOMBRE DE GRELS
! IN   RESUC1 : NOM DU CONCEPT RESULTAT DE LA COMMANDE CALC_ERREUR
!
!      SORTIE :
!-------------
!
! ......................................................................
!
!
!
    integer :: nel, iaux
    integer :: mode, ii, k, j, iad, idecgr
    integer :: ljeveu(6)
!
    character(len=1) :: kbid
    character(len=16) :: lpartr(6)
    character(len=16) :: lparst(3)
!
    real(kind=8) :: taberr(6), taberm(6), taber2(7)
!
!     ------------------------------------------------------------------
    data lpartr / 'ERRE_MEC_LOC' ,'ERRE_MEC_LOC_D' ,'ERRE_HYD_LOC',&
     &              'ERRE_MEC_GLOB','ERRE_MEC_GLOB_D','ERRE_HYD_GLOB' /
    data lparst / 'ERRE_MEC'     ,'ERRE_HYD_S'     ,'ERRE_HYD_D' /
!     ------------------------------------------------------------------
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    if (.not.perman) then
!
        call rsadpa(resuc1, 'L', 6, lpartr, iord-1,&
                    0, tjv=ljeveu, styp=kbid, istop=0)
!
        do 40 , iaux = 1 , 6
        if (iord .eq. 1) then
            taberm(iaux) = 0.d0
        else
            taberm(iaux) = zr(ljeveu(iaux))
        endif
 40     continue
    endif
!
! =======================================================
! 1. CALCUL DES TERMES GLOBAUX POUR CHACUN DES PARAMETRES
!    DE L'OPTION
! =======================================================
!
    do 10 , ii = 1,longt
!
    taber2(ii) = 0.d0
!
    do 20 , j = 1,nbgr
!
    mode=zi(jceld-1+zi(jceld-1+4+j) +2)
    if (mode .eq. 0) goto 20
    nel = nbelem(ligrel,j)
    idecgr=zi(jceld-1+zi(jceld-1+4+j)+8)
!
    do 30 , k = 1,nel
!
    iad = iavale-1+idecgr+(k-1)*longt
    taber2(ii) = taber2(ii) + zr(iad+ii-1)
!
 30 continue
!
 20 continue
!
    taber2(ii) = sqrt(taber2(ii))
!
    10 end do
!
! ==================================================
! 2. CALCUL DES TERMES GLOBAUX
! ==================================================
!
    if (perman) then
!
        taberr(1) = taber2(4)
        taberr(2) = taber2(3)
        taberr(3) = taber2(5)
!
    else
!
        taberr(1) = taber2(2)
        taberr(2) = taber2(3)
        taberr(3) = taber2(4)
        taberr(4) = max(taberm(4),taber2(2))
        taberr(5) = taberm(5) + taber2(3)
        taberr(6) = sqrt( taberm(6)**2 + taber2(4)**2 )
!
    endif
!
! ===================================================
! 3. ARCHIVAGE DES RESULTATS DANS LA SD RESULTAT
! ===================================================
!
    if (perman) then
!
        call rsadpa(resuc1, 'E', 3, lparst, iord,&
                    0, tjv=ljeveu, styp=kbid)
!
        do 50 , iaux = 1 , 3
        zr(ljeveu(iaux)) = taberr(iaux)
 50     continue
!
    else
!
        call rsadpa(resuc1, 'E', 6, lpartr, iord,&
                    0, tjv=ljeveu, styp=kbid)
!
        do 60 , iaux = 1 , 6
        zr(ljeveu(iaux)) = taberr(iaux)
 60     continue
!
    endif
!
    call jedema()
!
end subroutine
