subroutine drzrot(lisnoz, nbno, chargz, typlaz, lisrez,&
                  iocc, ndimmo)
    implicit none
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
#include "jeveux.h"
!
#include "asterc/getres.h"
#include "asterc/indik8.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/calirg.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/wkvect.h"
    character(len=*) :: chargz, lisnoz, typlaz, lisrez
    integer :: nbno, iocc, ndimmo
    character(len=8) :: charge
    character(len=19) :: lisrel
    character(len=24) :: lisnoe
! -------------------------------------------------------
!     LIAISON_SOLIDE +  ANGL_NAUT + TRAN
! -------------------------------------------------------
!  LISNOE        - IN    - K24 - : NOM DE LA LISTE DES NOEUDS
! -------------------------------------------------------
!  NBNO        - IN    - I   - :  NOMBRE DE NOEUDS
! -------------------------------------------------------
!  CHARGE        - IN    - K8  - : NOM DE LA SD CHARGE
! -------------------------------------------------------
! TYPLAG         - IN    - K2  - : TYPE DES MULTIPLICATEURS DE LAGRANGE
! -------------------------------------------------------
!  LISREL        - IN    - K19 - : NOM DE LA SD
!                - JXVAR -     -   LISTE DE RELATIONS
! -------------------------------------------------------
!  IOCC        - IN    - I - : NUMERO D'OCCURRENCE DE LIAISON_SOLIDE
! -------------------------------------------------------
!  NDIMMO      - IN    - I - : 2/3 : DIMENSION DU MODELE
! -------------------------------------------------------
!
!
! --------- VARIABLES LOCALES ---------------------------
    character(len=2) :: typlag
    character(len=8) :: resu
    character(len=8) :: mod, nomnoe, kbid
    character(len=8) :: noma, cmp, nomcmp(6)
    character(len=16) :: type, oper
    character(len=19) :: ligrmo
    character(len=24) :: geom2, linuno
    logical :: lrota
    real(kind=8) :: beta, rbid, mrota(3, 3)
    complex(kind=8) :: cbid
    integer :: jnoma, kcmp, ino, ilisno, i, ibid, jcoor, jprnm
    integer :: inom, ier, nbcmp, icmp, nbec, jnuno, jgeom2
! --------- FIN  DECLARATIONS  VARIABLES LOCALES --------
!
    call jemarq()
    call getres(resu, type, oper)
    lisrel = lisrez
    charge = chargz
    typlag = typlaz
    lisnoe = lisnoz
!
    nomcmp(1)='DX'
    nomcmp(2)='DY'
    nomcmp(3)='DZ'
!
!
    call dismoi('F', 'NOM_MODELE', charge(1:8), 'CHARGE', ibid,&
                mod, ier)
    ligrmo = mod(1:8)//'.MODELE'
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma = zk8(jnoma)
!
    call jeveuo(jexnom('&CATA.GD.NOMCMP', 'DEPL_R'), 'L', inom)
    call jelira(jexnom('&CATA.GD.NOMCMP', 'DEPL_R'), 'LONMAX', nbcmp, kbid)
    call dismoi('F', 'NB_EC', 'DEPL_R', 'GRANDEUR', nbec,&
                kbid, ier)
!
    call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    call jeveuo(lisnoe, 'L', ilisno)
!
!
!     TRANSFORMATION DE LA GEOMETRIE DES NOEUDS :
!     ------------------------------------------
    geom2 = '&&DRZROT.GEOM_TRANSF'
    linuno = '&&DRZROT.LINUNO'
    call wkvect(linuno, 'V V I', nbno, jnuno)
    do 12,i=1,nbno
    nomnoe=zk8(ilisno+i-1)
    call jenonu(jexnom(noma//'.NOMNOE', nomnoe), ino)
    zi(jnuno-1+i)=ino
    12 end do
    call calirg('LIAISON_SOLIDE', iocc, ndimmo, noma, linuno,&
                geom2, mrota, lrota)
    call jeveuo(geom2, 'L', jgeom2)
!
!
!     -- BOUCLE SUR LES NOEUDS :
!     ------------------------------
    do 40,i=1,nbno
    nomnoe=zk8(ilisno+i-1)
    ino=zi(jnuno-1+i)
!
!       -- BOUCLE SUR LES 3 CMPS : DX, DY, DZ
    do 41 , kcmp=1,3
    cmp= nomcmp(kcmp)
    icmp = indik8(zk8(inom),cmp,1,nbcmp)
    call assert(icmp.gt.0)
    if (exisdg(zi(jprnm-1+ (ino-1)*nbec+1),icmp)) then
!
        beta= zr(jgeom2-1+3*(ino-1)+kcmp) -zr(jcoor -1+3*(ino-&
                1)+kcmp)
        call afrela(1.d0, cbid, cmp, nomnoe, 0,&
                    rbid, 1, beta, cbid, ' ',&
                    'REEL', 'REEL', typlag, 0.d0, lisrel)
    endif
    41     end do
    40 end do
!
    call jedetr(geom2)
    call jedetr(linuno)
    call jedema()
end subroutine
