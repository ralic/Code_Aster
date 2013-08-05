subroutine xmrept(jcesd, jcesv, jcesl, izone, ndim,&
                  defico, geom, statue, mmait, amait,&
                  nmait)
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
!
    implicit none
#include "jeveux.h"
!
#include "asterc/r8gaem.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/xxmmvd.h"
    character(len=24) :: defico
    real(kind=8) :: geom(3)
    integer :: ndim, mmait, amait, nmait, statue, izone
    integer :: jcesd(10), jcesv(10), jcesl(10)
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (CONTACT - GRANDS GLISSEMENTS)
!
! ON CHERCHE LE POINT D'INTERSECTION MAITRE LE PLUS PROCHE DE
! POINT DE CONTACT
!
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
!
! ----------------------------------------------------------------------
!
!
!  JCES*(1)  : POINTEURS DE LA SD SIMPLE NB DE FACETTES ET DE PT D'INTER
!  JCES*(2)  : POINTEURS DE LA SD SIMPLE DES INFOS SUR ARETES COUPEES
!  JCES*(6)  : POINTEURS DE LA SD SIMPLE DES COOR DES PT D'INTER MAITRE
! IN  IZONE  : NUMÉRO DE ZONE DE LA MAILLE ESCLAVE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  NDIM   : DIMENSION DU MODELE
! IN  STATUE : STATUT DE LA MAILLE ESCLAVE
! IN  GEOM   : COORDONNEES DU POINT DE CONTACT ESCLAVE
! OUT MMAIT  : LE NUMÉRO GLOBAL DE LA MAILLE MAÎTRE CONTENANT LE POINT
!              D'INTERSECTION DE PLUS PROCHE
! OUT AMAIT  : LE NUMÉRO LOCAL DE L'ARRETE CONTENANT LE POINT
!              D'INTERSECTION DE PLUS PROCHE
! OUT NMAIT  : LE NUMÉRO LOCAL DU NOEUD CONTENANT LE POINT
!              D'INTERSECTION DE PLUS PROCHE
!
!
!
!
!
    integer :: nummai, ntmae, ima, iad, nbpt
    integer :: zmesx
    integer :: ini, j, i, ifiss
    real(kind=8) :: coord(3), dmin, dist
    character(len=24) :: maescx
    integer :: jmaesx
    integer :: zxain
!
! --------------------------------------------------------------------
!
    call jemarq()
!
! --- SI LA MAILLE ESCLAVE EST CRACK-TIP, ON SORT
!
    if (statue .eq. 2 .or. statue .lt. 0) goto 999
!
! --- INITIALISATIONS
!
    dmin = r8gaem()
    do 10 i = 1, 3
        coord(i)=0.d0
10  end do
    ntmae = cfdisi(defico,'NTMAE')
!
! --- RECUPERATION DE QUELQUES DONNEES
!
    maescx = defico(1:16)//'.MAESCX'
    call jeveuo(maescx, 'L', jmaesx)
    zmesx = cfmmvd('ZMESX')
    zxain = xxmmvd('ZXAIN')
!
! --- BOUCLE SUR LES MAILLES FISSURÉES
!
    do 100 ima = 1, ntmae
!
! --- SI CE N'EST PAS LA BONNE ZONE, ON SORT
!
        if (zi(jmaesx+zmesx*(ima-1)+2-1) .ne. izone) goto 100
!
! --- FAUX PT D'INTEGRATION
!
        if (zi(jmaesx+zmesx*(ima-1)+4-1) .lt. 0) goto 100
!
        nummai = zi(jmaesx+zmesx*(ima-1)+1-1)
        ifiss = zi(jmaesx+zmesx*(ima-1)+5-1)
!
! ----- RECUPERATION DU NOMBRE DE POINTS D'INTERSECTION DE LA MAILLE
        call cesexi('C', jcesd(1), jcesl(1), nummai, 1,&
                    ifiss, 3, iad)
        ASSERT(iad.gt.0)
        nbpt = zi(jcesv(1)-1+iad)
! ----- BOUCLE SUR LES POINTS D'INTERSECTION
!
        do 110 ini = 1, nbpt
! ------- COORDONNEES GEOMETRIQUES DU POINT D'INTERSECTION
            do 120 j = 1, ndim
                call cesexi('S', jcesd(6), jcesl(6), nummai, 1,&
                            ifiss, ndim*(ini-1)+j, iad)
                ASSERT(iad.gt.0)
                coord(j) = zr(jcesv(6)-1+iad)
120          continue
! ------- CALCUL DE LA DISTANCE
            dist = sqrt( ( coord(1)-geom(1))**2+ (coord(2)-geom(2))**2+ (coord(3)-geom(3) )**2 )
            call cesexi('S', jcesd(2), jcesl(2), nummai, 1,&
                        ifiss, zxain*( ini-1)+2, iad)
            if (nint(zr(jcesv(2)-1+iad)) .eq. 0 .and. ini .eq. 3 .and. ndim .eq. 2) goto 110
            if (dist .lt. dmin) then
                call cesexi('S', jcesd(2), jcesl(2), nummai, 1,&
                            ifiss, zxain*(ini-1)+1, iad)
                ASSERT(iad.gt.0)
                if (nint(zr(jcesv(2)-1+iad)) .gt. 0) then
                    amait = nint(zr(jcesv(2)-1+iad))
                    nmait = 0
                    dmin = dist
                    mmait = nummai
                else
                    call cesexi('S', jcesd(2), jcesl(2), nummai, 1,&
                                ifiss, zxain*(ini-1)+2, iad)
                    ASSERT(iad.gt.0)
                    if (nint(zr(jcesv(2)-1+iad)) .gt. 0) then
                        amait = 0
                        nmait = nint(zr(jcesv(2)-1+iad))
                        dmin = dist
                        mmait = nummai
                    endif
                endif
            endif
110      continue
100  end do
!
999  continue
!
    call jedema()
end subroutine
