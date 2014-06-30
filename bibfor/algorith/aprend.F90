subroutine aprend(sdappa)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/apcopt.h"
#include "asterfort/apnomk.h"
#include "asterfort/appari.h"
#include "asterfort/apsauv.h"
#include "asterfort/apzoni.h"
#include "asterfort/apzonl.h"
#include "asterfort/apzonr.h"
#include "asterfort/apzonv.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "blas/dcopy.h"
    character(len=19) :: sdappa
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (ALGO)
!
! RECHERCHE DU NOEUD MAITRE LE PLUS PROCHE DU POINT COURANT
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: apinfp, contno, coordo, defico, newgeo
    integer :: jinfp, jnoco, jcoor
    integer :: inom, izone, i, ip
    integer :: nbzone, ntpt
    integer :: nbnom, nbpt
    real(kind=8) :: coornm(3), coorpt(3)
    real(kind=8) :: distm, dist
    real(kind=8) :: normd, normv, dir(3), toleap
    real(kind=8) :: vecpml(3), vecpmm(3)
    integer :: jdecnm, numnom, posnom
    integer :: posmin, typapp
    logical(kind=1) :: dirapp, prtole, lexcl
    integer :: vali(2)
    real(kind=8) :: valr(4)
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<APPARIEMENT> RECH. NOEUD PLUS PROCHE'
    endif
!
! --- ACCES SDAPPA
!
    apinfp = sdappa(1:19)//'.INFP'
    call jeveuo(apinfp, 'L', jinfp)
    call apnomk(sdappa, 'DEFICO', defico)
    contno = defico(1:16)//'.NOEUCO'
    call jeveuo(contno, 'L', jnoco)
    call apnomk(sdappa, 'NEWGEO', newgeo)
    coordo = newgeo(1:19)//'.VALE'
    call jeveuo(coordo, 'L', jcoor)
!
! --- PARAMETRES
!
    call appari(sdappa, 'APPARI_NBZONE', nbzone)
    call appari(sdappa, 'APPARI_NTPT', ntpt)
!
! --- BOUCLE SUR LES ZONES
!
    ip = 1
    do 10 izone = 1, nbzone
!
! ----- INFORMATION SUR LA ZONE
!
        call apzoni(sdappa, izone, 'NBPT', nbpt)
        call apzoni(sdappa, izone, 'NBNOM', nbnom)
        call apzonl(sdappa, izone, 'DIRE_APPA_FIXE', dirapp)
        call apzoni(sdappa, izone, 'JDECNM', jdecnm)
        if (dirapp) then
            call apzonv(sdappa, izone, 'DIRE_APPA_VECT', dir)
        endif
        call apzonr(sdappa, izone, 'TOLE_APPA', toleap)
!
! ----- BOUCLE SUR LES POINTS
!
        do 20 i = 1, nbpt
!
! ------- INITIALISATIONS
!
            distm = r8gaem()
            prtole = .false.
            lexcl = .false.
            posmin = 0
            typapp = 0
!
! ------- COORDONNEES DU POINT
!
            call apcopt(sdappa, ip, coorpt)
!
! ------- NOEUD EXCLU OU PAS ?
!
            if (zi(jinfp+ip-1) .eq. 1) then
                lexcl = .true.
            endif
!
! ------- BOUCLE SUR LES NOEUDS MAITRES DE LA ZONE
!
            do 30 inom = 1, nbnom
!
! --------- POSITION DU NOEUD
!
                posnom = jdecnm + inom
!
! --------- NUMERO ABSOLU DU NOEUD
!
                numnom = zi(jnoco-1+posnom)
!
! --------- COORDONNEES DU NOEUD MAITRE
!
                coornm(1) = zr(jcoor+3*(numnom-1))
                coornm(2) = zr(jcoor+3*(numnom-1)+1)
                coornm(3) = zr(jcoor+3*(numnom-1)+2)
!
! --------- DISTANCE
!
                if (dirapp) then
                    normd = sqrt(dir(1)*dir(1)+ dir(2)*dir(2)+ dir(3)*dir(3))
                    normv = sqrt(&
                            (&
                            coorpt(1)-coornm(1))**2+ (coorpt(2)- coornm(2))**2+ (coorpt(3)-coornm&
                            &(3)&
                            )**2&
                            )
                    if (normv .eq. 0.d0) then
                        dist = 1.d0
                    else
                        dist = abs(&
                               (&
                               coorpt(1)-coornm(1))*dir(1)+ (coorpt(2)-coornm(2))*dir(2)+ (coorpt&
                               &(3)- coornm(3))*dir(3)&
                               )/(normd*normv&
                               )
                    endif
                else
                    dist = sqrt(&
                           (&
                           coorpt(1)-coornm(1))**2+ (coorpt(2)- coornm(2))**2+ (coorpt(3)-coornm(&
                           &3)&
                           )**2&
                           )
                endif
                vecpml(1) = coornm(1) - coorpt(1)
                vecpml(2) = coornm(2) - coorpt(2)
                vecpml(3) = coornm(3) - coorpt(3)
!
! --------- SELECTION
!
                if (dist .lt. distm) then
                    posmin = posnom
                    distm = dist
                    call dcopy(3, vecpml, 1, vecpmm, 1)
                    if (toleap .gt. 0.d0) then
                        if (dist .le. toleap) then
                            prtole = .true.
                        endif
                    else
                        prtole = .true.
                    endif
                endif
!
30          continue
!
! ------- APPARIEMENT HORS TOLE_APPA ?
!
            if (prtole) then
                typapp = 1
            else
                typapp = -2
            endif
!
! ------- NOEUD EXCLU
!
            if (lexcl) then
                typapp = -1
            endif
!
! ------- QUELLQUES VERIFS
!
            ASSERT(typapp.ne.0)
            ASSERT(posmin.ne.0)
!
! ------- PREPARATION STOCKAGE
!
            vali(1) = typapp
            vali(2) = posmin
            valr(1) = distm
            valr(2) = vecpmm(1)
            valr(3) = vecpmm(2)
            valr(4) = vecpmm(3)
!
! ------- STOCKAGE DE L'INFORMATION DANS SDAPPA
!
            call apsauv('ND_PROCHE', sdappa, izone, ip, vali,&
                        valr)
!
! ------- POINT SUIVANT
!
            ip = ip + 1
20      continue
10  end do
!
    ASSERT((ip-1).eq.ntpt)
!
    call jedema()
!
end subroutine
