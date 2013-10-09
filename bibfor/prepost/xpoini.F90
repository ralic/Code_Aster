subroutine xpoini(maxfem, modele, malini, modvis, licham,&
                  resuco, resux, prefno, nogrfi)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterfort/dismoi.h"
#include "asterfort/exixfe.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=2) :: prefno(4)
    character(len=8) :: maxfem, modele, malini, resuco, resux, modvis
    character(len=24) :: licham, nogrfi
!
!
!               RECUPERATION DES ENTREES SORTIES
!               POUR LES OPERATEURS DE POST-TRAITEMENT X-FEM
!
!
!   OUT
!       MAXFEM : MAILLAGE X-FEM
!       MODELE : MODELE FISSURE
!       MALINI : MAILLAGE SAIN
!       MODVIS : MODELE DE VISU (X-FEM)
!       LICHAM : LISTE DES CHAMPS A POST-TRAITER
!       RESUCO : NOM DU CONCEPT RESULTAT DONT ON EXTRAIT LES CHAMPS
!       RESUX  : NOM DU CONCEPT RESULTAT A CREER
!       PREFNO : PREFERENCES POUR LE NOMAGE DES NOUVELLES ENTITES
!       NOGRFI : NOM DU GROUPE DES NOEUDS SITUES SUR LA FISSURE
!
    integer :: iret, ibid, jlicha, jxc, i
    integer :: nbcham, nchmax
!     NOMBRE MAX DE CHAMPS A POST-TRAITER
    parameter    (nchmax=3)
    character(len=8) :: k8b
    character(len=16) :: k16b, nomcmd, tysd, linom(nchmax)
    character(len=19) :: k19bid
!
!
    call jemarq()
!
!     NOM DE LA COMMANDE (POST_MAIL_XFEM OU POST_CHAM_XFEM)
    call getres(k8b, k16b, nomcmd)
!
!     ------------------------------------------------------------------
    if (nomcmd .eq. 'POST_MAIL_XFEM') then
!     ------------------------------------------------------------------
!
!       NOM DU MAILLAGE DE SORTIE : MAXFEM
        call getres(maxfem, k16b, k16b)
!
!       MODELE ENRICHI : MODELE
        call getvid(' ', 'MODELE', scal=modele, nbret=iret)
        call exixfe(modele, iret)
        if (iret .eq. 0) then
            call utmess('F', 'XFEM_3', sk=modele)
        endif
!
!
!       PREFERENCES POUR LE NOMAGE DES NOUVELLES ENTITES
        call getvtx(' ', 'PREF_NOEUD_X', scal=prefno(1), nbret=ibid)
        call getvtx(' ', 'PREF_NOEUD_M', scal=prefno(2), nbret=ibid)
        call getvtx(' ', 'PREF_NOEUD_P', scal=prefno(3), nbret=ibid)
        call getvtx(' ', 'PREF_MAILLE_X', scal=prefno(4), nbret=ibid)
        call getvtx(' ', 'PREF_GROUP_CO', scal=nogrfi, nbret=ibid)
!
!     ------------------------------------------------------------------
    else if (nomcmd.eq.'POST_CHAM_XFEM') then
!     ------------------------------------------------------------------
!
!       NOM DE LA SD RESULTAT A CREER : RESUX
        call getres(resux, k16b, k16b)
!
!       MODELE DE VISU ET MAILLAGE DE VISU (X-FEM)
        call getvid(' ', 'MODELE_VISU', scal=modvis, nbret=iret)
        call dismoi('NOM_MAILLA', modvis, 'MODELE', repk=maxfem)
!
!       NOM ET TYPE DE LA SD RESULTAT EN ENTREE : RESUCO
        call getvid(' ', 'RESULTAT', scal=resuco, nbret=ibid)
!
!       MODELE ENRICHI ASSOCIE AU RESULTAT EN ENTREE
        call dismoi('NOM_MODELE', resuco, 'RESULTAT', repk=modele)
!
!       NOM DES CHAMPS A POST-TRAITER
        call gettco(resuco, tysd)
!
        if (tysd(1:9) .eq. 'MODE_MECA') then
            nbcham = 1
            linom(1) = 'DEPL'
        else if (tysd(1:9).eq.'EVOL_NOLI') then
!         A CORRIGER SUITE FICHE 15408
!         PB POST-TRAITEMENT VARIABLES INTERNES SI CONTACT P2P1 (GLUTE)
            call jeveuo(modele//'.XFEM_CONT', 'L', jxc)
            if (zi(jxc-1+1) .eq. 3) then
                write(6,*)'ON NE PEUT PAS POST-TRAITER LE CHAMP VARI_ELGA'
                nbcham = 2
                linom(1) = 'DEPL'
                linom(2) = 'SIEF_ELGA'
            else
                nbcham = 3
                linom(1) = 'DEPL'
                linom(2) = 'SIEF_ELGA'
                linom(3) = 'VARI_ELGA'
            endif
        else if (tysd(1:9).eq.'EVOL_ELAS') then
            call rsexch(' ', resuco, 'SIEF_ELGA', 1, k19bid,&
                        iret)
            if (iret .eq. 0) then
                nbcham = 2
                linom(1) = 'DEPL'
                linom(2) = 'SIEF_ELGA'
            else
                nbcham = 1
                linom(1) = 'DEPL'
            endif
        else if (tysd(1:9).eq.'EVOL_THER') then
            nbcham = 1
            linom(1) = 'TEMP'
        endif
!
        call wkvect(licham, 'V V K16', nbcham, jlicha)
        do 10 i = 1, nbcham
            zk16(jlicha-1+i)=linom(i)
 10     continue
!
!     ----------------------------------------------------------------
    endif
!     ------------------------------------------------------------------
!
!     MAILLAGE INITIAL : MALINI
!     CE MAILLAGE EST CELUI ASSOCIE AU MODELE ENRICHI
!     SAUF DANS LE CAS DU CONTACT AU ARETE 'P1P1A'
    call jeveuo(modele//'.XFEM_CONT', 'L', jxc)
!
!     MAILLAGE_SAIN NE SERT A RIEN :
!     RECUPERATION DU MAILLAGE ASSOCIE AU MODELE
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=malini)
!
    call jedema()
end subroutine
