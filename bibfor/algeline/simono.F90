subroutine simono()
    implicit none
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!
!     OPERATEUR :   CALC_CHAR_SEISME
!
!     CREE LE VECTEUR SECOND MEMBRE DANS LE CAS D'UN CALCUL SISMIQUE
!     STRUCTURE : MONO-APPUI
!
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/pteddl.h"
#include "asterfort/u2mess.h"
#include "asterfort/vtcrem.h"
#include "asterfort/wkvect.h"
    integer :: lmat, neq, ibid
    real(kind=8) :: xnorm, depl(6)
    character(len=8) :: tabcmp(6), masse, k8b
    character(len=14) :: nume
    character(len=16) :: type, nomcmd
    character(len=19) :: resu
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, idchm, ie, in, jddl, jvec, nbd
    integer :: nbdir, nbv
!-----------------------------------------------------------------------
    data   tabcmp / 'DX' , 'DY' , 'DZ' , 'DRX' , 'DRY' , 'DRZ' /
!     ------------------------------------------------------------------
!
! --- RECUPERATION DES ARGUMENTS DE LA COMMANDE
!
    call jemarq()
    resu = ' '
    call getres(resu, type, nomcmd)
!
! --- MATRICE DE MASSE
!
    call getvid(' ', 'MATR_MASS', scal=masse, nbret=nbv)
    call mtdscr(masse)
    call jeveuo(masse//'           .&INT', 'E', lmat)
    call dismoi('F', 'NOM_NUME_DDL', masse, 'MATR_ASSE', ibid,&
                nume, ie)
    call dismoi('F', 'NB_EQUA', masse, 'MATR_ASSE', neq,&
                k8b, ie)
!
! --- QUELLE EST LA DIRECTION ?
!
    call getvr8(' ', 'DIRECTION', nbval=0, nbret=nbd)
    nbdir = -nbd
    call getvr8(' ', 'DIRECTION', nbval=nbdir, vect=depl, nbret=nbd)
!
!     --- ON NORMALISE LE VECTEUR ---
    xnorm = 0.d0
    do 10 i = 1, nbdir
        xnorm = xnorm + depl(i) * depl(i)
10  end do
    xnorm = sqrt(xnorm)
    if (xnorm .lt. 0.d0) then
        call u2mess('F', 'ALGORITH9_81')
    endif
    do 12 i = 1, nbdir
        depl(i) = depl(i) / xnorm
12  end do
!
    call wkvect('&&SIMONO.VECTEUR', 'V V R', neq, jvec)
    call wkvect('&&SIMONO.DDL', 'V V I', neq*nbdir, jddl)
    call pteddl('NUME_DDL', nume, nbdir, tabcmp, neq,&
                zi(jddl))
    do 20 i = 1, nbdir
        do 22 in = 0, neq-1
            zr(jvec+in) = zr(jvec+in) - zi(jddl+(i-1)*neq+in)*depl(i)
22      continue
20  end do
!
!     --- CREATION DU CHAMNO ---
!
    call vtcrem(resu, masse, 'G', 'R')
    call jeveuo(resu//'.VALE', 'E', idchm)
!
    call mrmult('ZERO', lmat, zr(jvec), zr(idchm), 1,&
                .true.)
!
!      CALL WKVECT('&&SIMONO.DDL.BLOQUE','V V I',NEQ,IDDL)
!      CALL TYPDDL('BLOQ',NUME,NEQ,ZI(IDDL),NBACT,NBBLO,NBLAG,NBLIAI)
!      DO 40 IN = 0,NEQ-1
!         ZR(IDCHM+IN) = ( 1 - ZI(IDDL+IN) ) * ZR(IDCHM+IN)
! 40   CONTINUE
!
! --- MENAGE
    call jedetr('&&SIMONO.VECTEUR')
    call jedetr('&&SIMONO.DDL')
!
    call jedema()
end subroutine
