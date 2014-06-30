subroutine ccvepo(modele, resuin, lischa, nbchar, typesd,&
                  nbchre, ioccur, suropt, ligrel, exipou)
    implicit none
!     --- ARGUMENTS ---
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterfort/assert.h"
#include "asterfort/cochre.h"
#include "asterfort/copich.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlim1.h"
#include "asterfort/exlima.h"
#include "asterfort/gnomsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmamo.h"
#include "asterfort/utmess.h"
    logical(kind=1) :: exipou
    integer :: nbchre, ioccur, nbchar
    character(len=8) :: modele, resuin
    character(len=16) :: typesd
    character(len=19) :: lischa
    character(len=24) :: suropt, ligrel
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
! ----------------------------------------------------------------------
!  CALC_CHAMP - VERIFICATION POUR LES POUTRES
!  -    -       --                    --
! ----------------------------------------------------------------------
!
!  ROUTINE PERMETTANT DE SAVOIR SI DES POUTRES SONT DANS LE LIGREL
!   REDUIT ET DE VERIFIER LES CHARGES REPARTIES
!
! IN  :
!   MODELE  K8   NOM DU MODELE
!   RESUIN  K8   NOM DE LA STRUCTURE DE DONNEES RESULTAT IN
!   LISCHA  K19  NOM DE L'OBJET JEVEUX CONTENANT LES CHARGES
!   NCHARG  I    NOMBRE DE CHARGES
!   TYPESD  K16  TYPE DE LA STRUCTURE DE DONNEES RESULTAT
!
! OUT :
!   NBCHRE  I    NOMBRE DE CHARGES REPARTIES (POUTRES)
!   IOCCUR  I    NUMERO D'OCCURENCE OU SE TROUVE LE CHARGE REPARTIE
!   SUROPT  K24  SUROPTION
!   LIGREL  K24  NOM DU LIGREL A CREER
!   EXIPOU  L    LOGIQUE INDIQUANT LE PRESENCE DE POUTRES
! ----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
    integer :: ierd,  ltymo, nbmaal
    integer ::  n1, n2
!
    character(len=8) :: k8b
    character(len=16) :: typemo
    character(len=19) :: refe, masse, chdynr, chdepl
    character(len=24) :: noojb
    integer, pointer :: liste_mailles(:) => null()
    character(len=8), pointer :: lcha(:) => null()
!
    call jemarq()
!
    typemo = ' '
    suropt = ' '
    if (typesd .eq. 'MODE_MECA') then
        call rsadpa(resuin, 'L', 1, 'TYPE_MODE', 1,&
                    0, sjv=ltymo, styp=k8b)
        typemo=zk16(ltymo)
    endif
!
!     REDUCTION DU LIGREL SUR LA BASE DE GROUP_MA, GROUP_NO, ETC.
    n1 = getexm(' ','GROUP_MA')
    n2 = getexm(' ','MAILLE')
    if (n1+n2 .ne. 0) then
        call exlima(' ', 0, 'V', modele, ligrel)
    else
        call utmamo(modele, nbmaal, '&&CCVEPO.LISTE_MAILLES')
        call jeveuo('&&CCVEPO.LISTE_MAILLES', 'L', vi=liste_mailles)
        noojb='12345678.LIGR000000.LIEL'
        call gnomsd(' ', noojb, 14, 19)
        ligrel=noojb(1:19)
        ASSERT(ligrel.ne.' ')
        call exlim1(liste_mailles, nbmaal, modele, 'V', ligrel)
        call jedetr('&&CCVEPO.LISTE_MAILLES')
    endif
!
    call dismoi('EXI_POUX', ligrel, 'LIGREL', repk=k8b)
    exipou = .false.
!     SPECIAL POUTRE A LA POUX...
    if (k8b(1:3) .eq. 'OUI') then
        exipou = .true.
!       ON VERIFIE SI DERIERE LE TYPESD MODE_MECA ON TROUVE UN MODE_DYN
        if ((typesd.eq.'MODE_MECA'.and.typemo(1:8).eq.'MODE_DYN') .or. typesd .eq.&
            'DYNA_TRANS' .or. typesd .eq. 'MODE_ACOU' .or. typesd .eq. 'DYNA_HARMO') then
            refe=resuin
            suropt='MASS_MECA'
            call dismoi('REF_MASS_PREM', refe, 'RESU_DYNA', repk=masse, arret='C',&
                        ier=ierd)
            if (masse .ne. ' ') then
                call dismoi('SUR_OPTION', masse, 'MATR_ASSE', repk=suropt, arret='C',&
                            ier=ierd)
            endif
            call rsexch('F', resuin, 'DEPL', 1, chdepl,&
                        ierd)
            chdynr='&&MECALM.M.GAMMA'
            call copich('V', chdepl(1:19), chdynr)
        endif
        call jeveuo(lischa//'.LCHA', 'L', vk8=lcha)
!       VERIFIE L'UNICITE DE LA CHARGE REPARTIE
        ioccur=0
        call cochre(lcha, nbchar, nbchre, ioccur)
        if (nbchre .gt. 1) then
            call utmess('F', 'CALCULEL2_92')
        endif
    endif
!
    call jedema()
!
end subroutine
