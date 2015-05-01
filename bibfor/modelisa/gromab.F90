subroutine gromab(mailla, nmabet, nbmabe, mail2d, caelem,&
                  gromai)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rgcmpg.h"
#include "asterfort/utmess.h"
    character(len=8) :: mailla, caelem
    character(len=24) :: nmabet, gromai
    integer :: nbmabe
    aster_logical :: mail2d
!
! VARIABLES LOCALES
! -----------------
    integer :: ino, jcoor, nbno, ima, iad
    real(kind=8) :: xmax, xk, ymax, yk, zmax, zk, x, y, z, epmax
    real(kind=8) :: ep, sqrt
    character(len= 8) :: ngrand, nomai
    character(len=19) :: carte
    character(len=24) :: coorno, connex, k24bid, mommai
!
    integer :: i, j, k, iad2, inok
    integer :: jmabet, jconn, jtabco, jgmai
    integer :: nunoe(27)
    integer :: igrand, iasmax, iasedi, inomcp
    integer :: nbcmp, nbec, irep, iasbon, ii, icode, izone
    integer :: ilima, nbmaza, irvep, jj
    real(kind=8), pointer :: vale(:) => null()
    integer, pointer :: desc(:) => null()
!
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
!
!
    connex = mailla//'.CONNEX'
    coorno = mailla//'.COORDO    .VALE'
    mommai = mailla//'.NOMMAI'
    call jeveuo(coorno, 'L', jcoor)
!
    call jeveuo(connex, 'L', jconn)
    call jeveuo(jexatr(connex, 'LONCUM'), 'L', jtabco)
    call jeveuo(nmabet, 'L', jmabet)
    xmax=0.d0
    ymax=0.d0
    zmax=0.d0
    do i = 1, nbmabe
        ima = zi(jmabet-1+i)
        iad = zi(jtabco-1+ima)
        iad2 = zi(jtabco-1+ima+1)
        nbno = iad2-iad
        do j = 1, nbno
            ino = zi(jconn-1+iad-1+j)
            nunoe(j)=ino
        enddo
!
        do j = 1, nbno-1
            ino = nunoe(j)
            x = zr(jcoor+3*(ino-1) )
            y = zr(jcoor+3*(ino-1)+1)
            z = zr(jcoor+3*(ino-1)+2)
            do k = j+1, nbno
                inok = nunoe(k)
                xk = zr(jcoor+3*(inok-1) )
                yk = zr(jcoor+3*(inok-1)+1)
                zk = zr(jcoor+3*(inok-1)+2)
!
                if (abs(x-xk) .gt. xmax) xmax = abs(x-xk)
                if (abs(y-yk) .gt. ymax) ymax = abs(y-yk)
                if (abs(z-zk) .gt. zmax) zmax = abs(z-zk)
!
            enddo
        enddo
    end do
    call jeveuo(gromai, 'E', jgmai)
    zr(jgmai)=xmax
    zr(jgmai+1)=ymax
    zr(jgmai+2)=zmax
!
    j=0
    if (mail2d) then
!       DETERMINATION DE LA PLUS GRANDE EPAISSEUR
        carte=caelem//'.CARCOQUE  '
        call jeveuo(carte//'.DESC', 'L', vi=desc)
        call jeveuo(carte//'.VALE', 'L', vr=vale)
        igrand = desc(1)
        iasmax = desc(2)
        iasedi = desc(3)
        call jenuno(jexnum('&CATA.GD.NOMGD', igrand), ngrand)
        call jelira(jexnum('&CATA.GD.NOMCMP', igrand), 'LONMAX', nbcmp)
        call jeveuo(jexnum('&CATA.GD.NOMCMP', igrand), 'L', inomcp)
        call dismoi('NB_EC', ngrand, 'GRANDEUR', repi=nbec)
        irep = indik8( zk8(inomcp), 'EP' , 1, nbcmp )
        ASSERT(irep .ne. 0 )
!       BOUCLE SUR LES MAILLES
        epmax = 0.d0
        do i = 1, nbmabe
            ima = zi(jmabet-1+i)
!           RECHERCHE DE LA ZONE COMTENANT IMA
            iasbon = 0
            do ii = 1, iasedi
                icode = desc(1+3+2*(ii-1))
                izone = desc(1+3+2*(ii-1)+1)
!              SI C'EST UNE LISTE DE MAILLE
                if (icode .eq. 3) then
                    k24bid = carte//'.LIMA'
                    call jeveuo(jexnum(k24bid, izone), 'L', ilima)
                    call jelira(jexnum(k24bid, izone), 'LONMAX', nbmaza)
!              SI C'EST UN GROUPE DE MAILLE
                else if (icode.eq.2) then
                    k24bid = mailla//'.GROUPEMA'
                    call jeveuo(jexnum(k24bid, izone), 'L', ilima)
                    call jelira(jexnum(k24bid, izone), 'LONMAX', nbmaza)
!              SI C'EST TOUT LE MAILLAGE
                else if (icode.eq.1) then
                    iasbon = ii
                    goto 160
                else
                    ASSERT(.false.)
                endif
!              MAILLE DANS LISTE OU GROUPE DE MAILLE DE CETTE ZONE
                do jj = 1, nbmaza
                    if (ima .eq. zi(ilima+jj-1)) then
                        iasbon = ii
                        goto 160
                    endif
                end do
            end do
160         continue
            icode = desc(1+3+2*iasmax+nbec*(iasbon-1))
            irvep = rgcmpg(icode,irep)
            if (irvep .eq. 0) then
                call jenuno(jexnum(mommai, ima), nomai)
                call utmess('F', 'MODELISA8_3', sk=nomai)
            endif
            ep=vale(1+(iasbon-1)*nbcmp + irvep - 1)
            if (ep .gt. epmax) epmax = ep
        enddo
!
        epmax = epmax*sqrt(2.d0)
        do i = 1, 3
            if (zr(jgmai-1+i) .lt. epmax) zr(jgmai-1+i) = epmax
        enddo
!
    endif
!
    call jedema()
end subroutine
