subroutine ccaccl(option, modele, mateco, carael, ligrel,&
                  typesd, nbpain, lipain, lichin, lichou,&
                  codret)
    implicit none
!     --- ARGUMENTS ---
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cesvar.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecact.h"
#include "asterfort/mecara.h"
#include "asterfort/utmess.h"
!
    integer :: nbpain, codret
    character(len=8) :: modele, mateco, carael
    character(len=8) :: lipain(*)
    character(len=16) :: option, typesd
    character(len=24) :: lichin(*), ligrel, lichou(2)
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
!  CALC_CHAMP - AJOUT ET CALCUL DE CHAMPS LOCAUX
!  -    -       -        -         -      -
! ----------------------------------------------------------------------
!
!  ROUTINE DE GESTION DES GLUTES NECESSAIRES POUR CERTAINES OPTIONS
!  * POUTRE POUX, DCHA_*, RADI_*, ...
!  * APPEL A CESVAR POUR LES CHAMPS A SOUS-POINTS.
!
! IN  :
!   OPTION  K16  NOM DE L'OPTION
!   MODELE  K8   NOM DU MODELE
!   RESUIN  K8   NOM DE LA STRUCUTRE DE DONNEES RESULTAT IN
!   MATECO  K8   NOM DU MATERIAU CODE
!   CARAEL  K8   NOM DU CARAELE
!   LIGREL  K24  NOM DU LIGREL
!   NUMORD  I    NUMERO D'ORDRE COURANT
!   NORDM1  I    NUMERO D'ORDRE PRECEDENT
!   TYPESD  K16  TYPE DE LA STRUCTURE DE DONNEES RESULTAT
!   NBPAIN  I    NOMBRE DE PARAMETRES IN
!   LIPAIN  K8*  LISTE DES PARAMETRES IN
!   EXITIM  BOOL EXISTENCE OU NON DE L'INSTANT DANS LA SD RESULTAT
!   LICHOU  K24* LISTE DES CHAMPS OUT
!
! IN/OUT :
!   LICHIN  K24* LISTE MODIFIEE DES CHAMPS IN
! ----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
!
    integer :: iret1, iret2, kparin
    integer :: ipara, inume, nbsp
    character(len=8) :: k8b, noma, curpar, carae2, parain
    character(len=16) :: concep, nomcmd
    character(len=19) :: compor, compo2, canbva
    character(len=24) :: chnlin
    character(len=24) :: chcara(18)
!
    call jemarq()
!
    codret=0
!
    call getres(k8b, concep, nomcmd)
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=noma)
!
!
    call mecara(carael, chcara)
    if (carael(1:8) .ne. ' ') then
        do ipara = 1, nbpain
            curpar=lipain(ipara)
            if (curpar .eq. 'PCACOQU') lichin(ipara)=chcara(7)
        end do
    endif
!
    if (option .eq. 'SIEQ_ELGA') then
        if (typesd .eq. 'FOURIER_ELAS') then
            call utmess('F', 'CALCULEL6_83', sk=option)
        endif
    endif
!
!     -- GLUTE EFGE_ELNO (J. PELLET) :
!     --------------------------------
    if (option .eq. 'EFGE_ELNO') then
        chnlin='&&CCACCL.PNONLIN'
!       -- INUME=0 => CALCUL LINEAIRE
!       -- INUME=1 => CALCUL NON-LINEAIRE
        inume=0
        if (typesd .eq. 'EVOL_NOLI') inume=1
        call mecact('V', chnlin, 'MAILLA', noma, 'NEUT_I',&
                    ncmp=1, nomcmp='X1', si=inume)
!       -- SI LINEAIRE, ON DOIT CHANGER PCOMPOR (POUR POU_D_EM):
        if (inume .eq. 0) then
            kparin=indik8(lipain,'PCOMPOR',1,nbpain)
            ASSERT(kparin.ge.1)
            lichin(kparin)=mateco(1:8)//'.COMPOR'
        endif
!
    else if (option.eq.'VARI_ELNO') then
!     -- POUR CETTE OPTION ON A BESOIN DE COMPOR :
        do ipara = 1, nbpain
            curpar=lipain(ipara)
            if (curpar .eq. 'PCOMPOR') compor=lichin(ipara)(1:19)
        end do
        call exisd('CARTE', compor, iret2)
        if (iret2 .ne. 1) then
            call utmess('A', 'CALCULEL2_86')
            codret=1
            goto 30
!
        endif
    endif
!
!
!     ---------------------------------------------------------------
!     -- AJOUT EVENTUEL DU CHAM_ELEM_S PERMETTANT LES SOUS-POINTS
!        ET LE BON NOMBRE DE VARIABLES INTERNES
!     ---------------------------------------------------------------
!
    if ((option.eq.'EPEQ_ELGA') .or. (option.eq.'EPEQ_ELNO') .or. (option.eq.'EPSI_ELGA')&
        .or. (option.eq.'EPSI_ELNO') .or. (option.eq.'SIEF_ELGA') .or.&
        (option.eq.'SIEF_ELNO') .or. (option.eq.'SIEQ_ELGA') .or. (option.eq.'SIEQ_ELNO')&
        .or. (option.eq.'SIGM_ELGA') .or. (option.eq.'SIGM_ELNO') .or.&
        (option.eq.'VARI_ELNO')) then
!
!       -- CONCERNANT LES VARIABLES INTERNES :
        if (option .eq. 'VARI_ELNO') then
            compo2=compor
        else
            compo2=' '
        endif
!
        carae2=carael
!
!       -- POUR LES OPTIONS SUIVANTES, LE NOMBRE DE SOUS-POINTS
!          DU CHAMP "OUT" DEPEND D'UN CHAMP "IN" PARTICULIER :
        if (option .eq. 'EPEQ_ELGA') then
            parain='PDEFORR'
        else if (option.eq.'EPEQ_ELNO') then
            parain='PDEFORR'
        else if (option.eq.'EPSI_ELNO') then
            parain='PDEFOPG'
        else if (option.eq.'SIEF_ELNO') then
            parain='PCONTRR'
        else if (option.eq.'SIEQ_ELGA') then
            parain='PCONTRR'
        else if (option.eq.'SIEQ_ELNO') then
            parain='PCONTRR'
        else if (option.eq.'SIGM_ELNO') then
            parain='PCONTRR'
        else if (option.eq.'SIGM_ELGA') then
            parain='PSIEFR'
        else
            parain=' '
        endif
!
        if (parain .ne. ' ') then
            kparin=indik8(lipain,parain,1,nbpain)
            ASSERT(kparin.ge.1)
            call jeexin(lichin(kparin)(1:19)//'.CELD', iret1)
            nbsp=1
            if (iret1 .ne. 0) then
                call dismoi('MXNBSP', lichin(kparin), 'CHAM_ELEM', repi=nbsp)
            endif
            if (nbsp .le. 1) carae2=' '
        endif
!
        canbva='&&CCACCL.CANBVA'
        if (carae2 .ne. ' ' .or. compo2 .ne. ' ') then
            call cesvar(carae2, compo2, ligrel, canbva)
            call copisd('CHAM_ELEM_S', 'V', canbva, lichou(1))
            call detrsd('CHAM_ELEM_S', canbva)
        endif
    endif
!
!
 30 continue
    call jedema()
!
end subroutine
