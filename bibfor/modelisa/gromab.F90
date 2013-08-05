subroutine gromab(mailla, nmabet, nbmabe, mail2d, caelem,&
                  gromai)
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
!-----------------------------------------------------------------------
!  DESCRIPTION : RECHERCHE DES PLUS GRANDS DIAMETRES DES MAILLES DE LA
!  -----------   LISTE NMABET SELON LES DIRECTIONS X, Y ET Z
!  IN     : MAILLA : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT MAILLAGE ASSOCIE A L'ETUDE
!  IN     : NMABET : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR D'ENTIERS POUR STOCKAGE DES
!                    NUMEROS DE MAILLES APPARTENANT A LA STRUCTURE BETON
!  IN     : NBMABE : INTEGER , SCALAIRE
!                    NOMBRE DE MAILLES APPARTENANT A LA STRUCTURE BETON
!  IN     : MAIL2D : LOGICAL , SCALAIRE
!                    .TRUE. SI LES MAILLES SONT DES MAILLES 2D
!  IN     : CAELEM : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT CARA_ELEM ASSOCIE A L'ETUDE
!  OUT    : NUNOBI : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR DE REELS POUR STOCKAGE DES
!                    3 PLUS GRANDS DIAMETRES RESP SELON X, Y ET Z
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/wkvect.h"
    character(len=8) :: mailla, caelem
    character(len=24) :: nmabet, gromai
    integer :: nbmabe
    logical :: mail2d
!
! VARIABLES LOCALES
! -----------------
    integer :: ino, iret, jcoor, nbno, ima, iad
    real(kind=8) :: xmax, xk, ymax, yk, zmax, zk, x, y, z, epmax
    real(kind=8) :: ep, sqrt
    character(len=1) :: k1b
    character(len=19) :: carte
    character(len=24) :: coorno, connex, cavale
!
    integer :: i, j, k, iad2, inok
    integer :: jmabet, jconn, jtabco, jnunoe, jgmai, ival, ncaco
    integer :: ncava, nep
!
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
!
!
    connex = mailla//'.CONNEX'
    coorno = mailla//'.COORDO    .VALE'
    call jeveuo(coorno, 'L', jcoor)
!
    call jeveuo(connex, 'L', jconn)
    call jeveuo(jexatr(connex, 'LONCUM'), 'L', jtabco)
    call jeveuo(nmabet, 'L', jmabet)
    call wkvect('&&GROMAB.NUNOE', 'V V I', 27, jnunoe)
    xmax=0.d0
    ymax=0.d0
    zmax=0.d0
    do 10 i = 1, nbmabe
        ima = zi(jmabet-1+i)
        iad = zi(jtabco-1+ima)
        iad2 = zi(jtabco-1+ima+1)
        nbno = iad2-iad
        do 20 j = 1, nbno
            ino = zi(jconn-1+iad-1+j)
            zi(jnunoe-1+j)=ino
20      continue
!
        do 30 j = 1, nbno-1
            ino = zi(jnunoe-1+j)
            x = zr(jcoor+3*(ino-1) )
            y = zr(jcoor+3*(ino-1)+1)
            z = zr(jcoor+3*(ino-1)+2)
            do 40 k = j+1, nbno
                inok = zi(jnunoe-1+k)
                xk = zr(jcoor+3*(inok-1) )
                yk = zr(jcoor+3*(inok-1)+1)
                zk = zr(jcoor+3*(inok-1)+2)
!
                if (abs(x-xk) .gt. xmax) xmax = abs(x-xk)
                if (abs(y-yk) .gt. ymax) ymax = abs(y-yk)
                if (abs(z-zk) .gt. zmax) zmax = abs(z-zk)
!
40          continue
30      continue
10  end do
    call jeveuo(gromai, 'E', jgmai)
    zr(jgmai)=xmax
    zr(jgmai+1)=ymax
    zr(jgmai+2)=zmax
!
    j=0
    if (mail2d) then
!       DETERMINATION DE LA PLUS GRANDE EPAISSEUR
        carte=caelem//'.CARCOQUE  '
        cavale = carte//'.VALE'
        call jeexin(cavale, iret)
        ASSERT(iret.ne.0)
!
        call jelira(jexnom('&CATA.GD.NOMCMP', 'CACOQU'), 'LONMAX', ncaco, k1b)
!
        call jelira(cavale, 'LONMAX', ncava, k1b)
        call jeveuo(cavale, 'L', ival)
!
        nep = ncava/ncaco
        epmax = 0.d0
        do 50 i = 1, nep
            ep = zr(ival+(i-1)*ncaco)
            if (ep .gt. epmax) epmax = ep
50      continue
        epmax = epmax*sqrt(2.d0)
        do 60 i = 1, 3
            if (zr(jgmai-1+i) .lt. epmax) zr(jgmai-1+i) = epmax
60      continue
!
    endif
!
    call jedema()
end subroutine
