subroutine cglemu(resu, vecord, lmelas, lncas, melord)
    implicit none
!
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/getvtx.h"
#include "asterfort/indiis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsorac.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: resu
    character(len=19) :: vecord
    character(len=24) :: melord
    logical :: lmelas, lncas
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
!     BUT : LECTURE DES INFORMATINOS RELATIVES AUX MULT_ELAS
!  IN :
!     RESU   : MOT-CLE RESULTAT
!     VECORD : VECTEUR DES NUME_ORDRE DU RESU
!  OUT :
!     LMELAS : .TRUE. SI LA SD_RESULTAT EST DE TYPE MULT_ELAS
!     LNCAS  : .TRUE. SI ON A RENSEIGNE DANS CALC_G LES CAS A TRAITER
!     MELORD : VECTEUR DE LOGICAL POUR SAVOIR SI UN NUME_ORDRE
!              DOIT ETRE TRAITE OU PAS
! ======================================================================
!
    integer :: ier, ncas, jncas, jnord, i, ibid, iind, ivec, nbord
    integer :: nutrou, nbtrou
    real(kind=8) :: rbid
    character(len=8) :: k8b
    character(len=16) :: typsd, nomcas, valk(2)
    character(len=24) :: melcas
    complex(kind=8) :: c16b
!
    call jemarq()
!
!     INITIALISATIONS
    lncas =.false.
    lmelas=.false.
!
    call jeveuo(vecord, 'L', ivec)
    call jelira(vecord, 'LONMAX', nbord)
!
    call gettco(resu, typsd)
!
    if (typsd .ne. 'MULT_ELAS') goto 999
!
    lmelas=.true.
!
!     A T-ON RENSEIGNE NOM_CAS DANS CALC_G ?
    call getvtx(' ', 'NOM_CAS', nbval=0, nbret=ncas)
!
    if (ncas .ne. 0) then
!
!       NOM_CAS EST RENSEIGNE DANS CALC_G
        lncas=.true.
!
!       NOMBRE DE CAS A TRAITER
        ncas=-ncas
!
!       CREATION DU VECTEUR CONTENANT LES NOMS DES CAS
        melcas = '&&CGLEMU.MULTELAS.NOMCAS'
        call wkvect(melcas, 'V V K16', ncas, jncas)
!
!       REMPLISSAGE DU VECTEUR CONTENANT LES NOMS DES CAS
        call getvtx(' ', 'NOM_CAS', nbval=ncas, vect=zk16(jncas), nbret=ier)
!
!       CREATION DU VECTEUR DE LOGICAL POUR SAVOIR SI UN NUME_ORDRE
!       DOIT ETRE TRAITE OU PAS
        call wkvect(melord, 'V V L', nbord, jnord)
!
!       REMPLISSAGE DU VECTEUR MELORD
        do 10 i = 1, ncas
!         RECUP DU NUME_ORDRE CORRESPONDANT AU NOM_CAS COURANT
            nomcas=zk16(jncas-1+i)
            call rsorac(resu, 'NOM_CAS', ibid, rbid, nomcas,&
                        c16b, rbid, k8b, nutrou, 1,&
                        nbtrou)
            if (nbtrou .eq. 0) then
                valk(1)=nomcas
                valk(2)=resu
                call utmess('F', 'RUPTURE0_28', nk=2, valk=valk)
            else
                iind=indiis(zi(ivec),nutrou,1,nbord)
                zl(jnord+iind-1)=.true.
            endif
10      continue
!
        call jedetr(melcas)
!
    endif
!
999  continue
!
    call jedema()
!
end subroutine
