subroutine cglecc(typfis, resu, vecord, calsig)
    implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mess.h"
!
    character(len=8) :: typfis, resu, calsig
    character(len=19) :: vecord
!
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
! person_in_charge: samuel.geniaut at edf.fr
!
!     SOUS-ROUTINE DE L'OPERATEUR CALC_G
!
!     BUT : LECTURE DE LA CARTE DE COMPORTEMENT UTILISEE DANS LE CALCUL
!
!  IN :
!     TYPFIS : TYPE D'OBJET POUR DECRIRE LE FOND DE FISSURE
!              'FONDFISS' OU 'FISSURE' OU 'THETA'
!     RESU   : MOT-CLE RESULTAT
!     VECORD : VECTEUR DES NUME_ORDRE DU RESU
!  OUT :
!     CALSIG : 'OUI' S'IL FAUT RECALCULER LES CONTRAITNES
!              'NON' S'IL NE FAUT PAS RECALCULER LES CONTRAINTES
! ======================================================================
!
    integer :: ncelas, ier, i, jvec, nbord, iord, vali
    character(len=24) :: k24b
!
    call jemarq()
!
!     RECUPERATION DU MOT-CLE CALCUL_CONTRAINTE
!
!     PAR DEFAUT, ON RECALCULE LES CONTRAINTES
    calsig='OUI'
!
    call getfac('COMP_ELAS', ncelas)
!
    if (ncelas .gt. 0) then
        call getvtx(' ', 'CALCUL_CONTRAINTE', scal=calsig, nbret=ier)
    endif
!
!     CALSIG='NON' N'EST PAS COMPATIBLE AVEC X-FEM
    if (calsig .eq. 'NON') then
        if (typfis .eq. 'FISSURE') call u2mess('F', 'RUPTURE1_39')
    endif
!
!     LES AUTRES VERIF SONF FAITES DANS LE CAPY (OPTION...)
!
!     VERIFICATION DE LA PRESENCE DU CHAMP SIEF_ELGA
!     LORSQUE CALCUL_CONTRAINTE='NON', SINON ERREUR 'F'
    if (calsig .eq. 'NON') then
!
        call jeveuo(vecord, 'L', jvec)
        call jelira(vecord, 'LONMAX', nbord)
!
        do 10 i = 1, nbord
            iord = zi(jvec-1+i)
            call rsexch(' ', resu, 'SIEF_ELGA', iord, k24b,&
                        ier)
            if (ier .ne. 0) then
!           PROBLEME DANS LA RECUP DE SIEF_ELGA POUR CE NUME_ORDRE
                vali=iord
                call u2mesi('F', 'RUPTURE0_93', 1, vali)
            endif
10      continue
    endif
!
    call jedema()
!
end subroutine
