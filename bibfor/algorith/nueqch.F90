subroutine nueqch(erreur, chamno, noma, nbno, numno,&
                  nomcmp, nueq)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
!
#include "asterc/indik8.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/u2mesk.h"
    character(len=19) :: chamno
    character(len=8) :: nomcmp(*), noma
    character(len=1) :: erreur
    integer :: nbno, numno(*), nueq(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE)
!
! PERMET DE RECUPERER LES NUMEROS DES EQUATIONS DANS LE .VALE A
! PARTIR DES NOMS DE NOEUD ET DES NOMS DE COMPOSANTE
!
! ----------------------------------------------------------------------
!
!
! IN  CHAMNO  : CHAM_NO A MODIFIER
! IN  ERREUR  : 'F' SI UNE COMPOSANTE ABSENTE -> ERREUR
!               'A' SI UNE COMPOSANTE ABSENTE -> ALARME
!               ' ' SI UNE COMPOSANTE ABSENTE -> RIEN
! IN  NBNO    : NOMBRE DE NOEUDS
! IN  NUMNO   : LISTE DES NUMEROS DE NOEUD
! IN  NOMCMP  : LISTE DES NOMS DE COMPOSANTE
! OUT NUEQ    : LISTE DES POSITIONS DANS LE .VALE
!
!
!
!
    character(len=19) :: pfchno
    character(len=8) :: nomgd
    integer :: ibid, ier, nbcmpx, ncmp, ico, itrou
    integer :: icmp, ino, idc, nec
    integer :: jcmp, jnueq, jprno
    logical :: exis
    character(len=8) :: k8bid, nom
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call dismoi('F', 'NOM_GD', chamno, 'CHAM_NO', ibid,&
                nomgd, ier)
    call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nec,&
                k8bid, ier)
    call dismoi('F', 'NB_CMP_MAX', nomgd, 'GRANDEUR', nbcmpx,&
                k8bid, ier)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jcmp)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', ncmp)
!
    call dismoi('F', 'PROF_CHNO', chamno, 'CHAM_NO', ibid,&
                pfchno, ier)
    call jeveuo(pfchno//'.PRNO', 'L', jprno)
    call jeveuo(pfchno//'.NUEQ', 'L', jnueq)
!
    do 10 ino = 1, nbno
        nom = nomcmp(ino)
        idc = indik8(zk8(jcmp),nom ,1,ncmp)
        ico = 0
        do 100 icmp = 1, nbcmpx
            if (exisdg(zi(jprno-1+(nec+2)*(numno(ino)-1)+2+1),icmp)) then
                ico = ico + 1
                exis = .true.
            else
                exis = .false.
            endif
            if (icmp .eq. idc) then
                if (exis) then
                    itrou = ico
                    goto 101
                else
                    itrou = 0
                    if (erreur .ne. ' ') then
                        call u2mesk(erreur, 'MECANONLINE5_50', 1, nom)
                    endif
                endif
            endif
100      continue
        if (erreur .ne. ' ') then
            call u2mesk(erreur, 'MECANONLINE5_50', 1, nom)
        endif
101      continue
        if (itrou .eq. 0) then
            nueq(ino) = 0
        else
            nueq(ino) = zi( jnueq-1+zi(jprno-1+ (nec+2)*(numno(ino)-1)+ 1)+itrou-1)
        endif
10  end do
!
    call jedema()
end subroutine
