subroutine ccchno(option, numord, resuin, resuou, lichou,&
                  mesmai, nomail, modele, carael, basopt,&
                  ligrel, ligmod, codret)
    implicit none
!     --- ARGUMENTS ---
#include "jeveux.h"
#include "asterfort/ccvrrl.h"
#include "asterfort/celces.h"
#include "asterfort/cescns.h"
#include "asterfort/cesred.h"
#include "asterfort/cnscno.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/inigrl.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexc1.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
!
    integer :: numord, codret
    character(len=1) :: basopt
    character(len=8) :: resuin, resuou, nomail, modele, carael
    character(len=16) :: option
    character(len=24) :: lichou(2), mesmai, ligrel
    logical :: ligmod
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
! ----------------------------------------------------------------------
!  CALC_CHAMP - CALCUL D'UN CHAMP AUX NOEUDS
!  -    -                   --        --
! ----------------------------------------------------------------------
!
!  ROUTINE DE CALCUL D'UN CHAMP AUX NOEUDS DE CALC_CHAMP
!
! IN  :
!   OPTION  K16  NOM DE L'OPTION A CALCULER
!   NUMORD  I    NUMERO D'ORDRE COURANT
!   RESUIN  K8   NOM DE LA STRUCUTRE DE DONNEES RESULTAT IN
!   RESUOU  K8   NOM DE LA STRUCUTRE DE DONNEES RESULTAT OUT
!   MESMAI  K24  NOM DU VECTEUR CONTENANT LES MAILLES SUR LESQUELLES
!                LE CALCUL EST DEMANDE
!   NOMAIL  K8   NOM DU MAILLAGE SUR LEQUEL LE CALCUL EST REALISE
!   MODELE  K8   NOM DU MODELE
!   CARAEL  K8   NOM DU CARAEL
!   BASOPT  K1   BASE SUR LAQUELLE DOIT ETRE CREE LE CHAMP DE SORTIE
!   LIGREL  K24  NOM DU LIGREL
!
! IN/OUT :
!   LICHOU  K8*  LISTE DES CHAMPS OUT
! ----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
    integer :: ier, nbma, jmai, ngr, igr, nmaxob, iret
    parameter    (nmaxob=30)
    integer :: adobj(nmaxob), nbobj, nbsp
!
    character(len=8) :: k8b, erdm
    character(len=16) :: valk(4)
    character(len=19) :: optele, nochou, chams0, chams1
    character(len=24) :: chelem, noobj(nmaxob)
!
!
    call jemarq()
!
    call jeexin(mesmai, ier)
    if (ier .ne. 0) then
        call jeveuo(mesmai, 'L', jmai)
        call jelira(mesmai, 'LONMAX', nbma)
    else
        nbma = 0
    endif
!
    chams0 = '&&CALCOP.CHAMS0'
    chams1 = '&&CALCOP.CHAMS1'
    optele = option(1:5)//'ELNO'
!
    call rsexc1(resuou, option, numord, nochou)
    lichou(1) = nochou
!
    call rsexch(' ', resuin, optele, numord, chelem,&
                ier)
    if (ier .ne. 0) then
        call rsexch(' ', resuou, optele, numord, chelem,&
                    ier)
    endif
!
    call exisd('CHAMP_GD', chelem, ier)
    if (ier .eq. 0) then
        lichou=' '
        valk(1)=optele
        valk(2)=option
        valk(3)=resuin
        valk(4)=resuou
        call utmess('A', 'CALCCHAMP_2', nk=4, valk=valk, si=numord)
        goto 9999
    endif
    call celces(chelem, 'V', chams0)
    if (nbma .ne. 0) then
        call cesred(chams0, nbma, zi(jmai), 0, [k8b],&
                    'V', chams0)
    endif
!
!     VERIFICATION DES REPERES LOCAUX
    erdm = 'NON'
    call dismoi('EXI_RDM', ligrel, 'LIGREL', repk=erdm)
!
!     CETTE VERIFICATION NE DOIT ETRE FAITE QUE DANS LE CAS
!     OU LE MODELE CONTIENT DE ELEMENTS DE STRUCTURE
!     ET QUE POUR CERTAINS CHAMPS QUI SONT EN REPERE LOCAL
    if ((erdm.eq.'OUI') .and.&
        (&
        (option(1:4).eq.'EPSI') .or. (option(1:4).eq.'SIGM') .or. (option(1:4).eq.'DEGE')&
        .or. (option(1:4).eq.'EFGE')&
        )) then
        if (ligmod) then
!
!         POUR LES COQUES 3D CERTAINES INITIALISATIONS SONT
!         NECESSAIRES POUR POUVOIR UTILISER LES ROUTINES
!         DE CHANGEMENT DE REPERE PROPRES AUX COQUES 3D
            call dismoi('EXI_COQ3D', ligrel, 'LIGREL', repk=erdm)
            if (erdm .eq. 'OUI' .and. ligmod) then
                call jelira(ligrel(1:19)//'.LIEL', 'NUTIOC', ngr)
                do 10 igr = 1, ngr
                    call inigrl(ligrel, igr, nmaxob, adobj, noobj,&
                                nbobj)
 10             continue
            endif
        endif
        if (carael .ne. ' ') call ccvrrl(nomail, modele, carael, mesmai, chams0,&
                                         'A', codret)
    endif
!
    call cescns(chams0, ' ', 'V', chams1, 'A',&
                codret)
    call cnscno(chams1, ' ', 'NON', basopt, nochou,&
                ' ', iret)
!
!     VERIFICATION POUR LES CHAMPS A SOUS-POINT
    call dismoi('MXNBSP', chelem, 'CHAM_ELEM', repi=nbsp)
    if ((nbsp.gt.1) .and. (iret.eq.1)) then
        valk(1)=optele
        valk(2)=option
        call utmess('F', 'CALCULEL4_16', nk=2, valk=valk)
    endif
!
    call detrsd('CHAM_ELEM_S', chams0)
    call detrsd('CHAM_NO_S', chams1)
!
9999 continue
!
    call jedema()
!
end subroutine
