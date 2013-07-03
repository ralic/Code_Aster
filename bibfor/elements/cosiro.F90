subroutine cosiro(nomte, param, loue, sens, goun,&
                  jtens, sour)
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
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
! BUT : CHANGER LE REPERE : INTRINSEQUE <-> UTILISATEUR
!       POUR UN CHAMP LOCAL DE CONTRAINTE (OU DE DEFORMATION)
!       (MODELISATIONS : DKT/DST/Q4G/COQUE_3D)
!
! ARGUMENTS :
!  NOMTE  IN : NOM DU TYPE_ELEMENT
!  PARAM  IN : NOM DU CHAMP LOCAL A MODIFIER
!  LOUE   IN :  / 'L' : PARAMETRE EN LECTURE
!               / 'E' : PARAMETRE EN ECRITURE
!  SENS   IN :  / 'IU' : INTRINSEQUE -> UTILISATEUR
!               / 'UI' : UTILISATEUR -> INTRINSEQUE
!  GOUN   IN :  / 'G' : CHAMP ELGA
!               / 'N' : CHAMP ELNO
!  JTENS  OUT : ADRESSE DU CHAMP LOCAL (QUE L'ON A MODIFIE)
!  SOUR   IN :  / 'S' : ON CALCULE LE CHANGEMENT DE REPERE
!                       ET ON LE CONSERVE (SAVE)
!               / 'R' : ON REUTILISE LE CHANGEMENT DE REPERE
!                       CALCULE (ET SAUVE) PRECEDEMMENT
!  REMARQUE : UTILISER SOUR='R' PEUT FAIRE GAGNER UN PEU DE TEMPS MAIS
!             CELA PERMET SURTOUT DE SE PROTEGER DES TE00IJ QUI
!             MODIFIENT LA GEOMETRIE INITIALE (EX : TE0031)
! ======================================================================
    implicit none
!
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/assert.h"
#include "asterfort/coqrep.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxsiro.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/vdrepe.h"
#include "asterfort/vdsiro.h"
#include "asterfort/vdxrep.h"
    character(len=*) :: param
    character(len=16) :: nomte
    character(len=2) :: sens
    character(len=1) :: loue, goun, sour
    integer :: jtens, npgt, jgeom, jcara
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano
    integer :: itab(7), iret, nbpt, nbsp
    parameter      (npgt=10)
    real(kind=8) :: matevn(2, 2, npgt), matevg(2, 2, npgt)
    real(kind=8) :: t2ev(4), t2ve(4), pgl(3, 3), epais, alpha, beta
    real(kind=8) :: c, s
    save           t2ev,t2ve,matevn,matevg
!
    call assert(loue.eq.'L' .or. loue.eq.'E')
    call assert(sens.eq.'UI' .or. sens.eq.'IU')
    call assert(sour.eq.'S' .or. sour.eq.'R')
    call assert(goun.eq.'G' .or. goun.eq.'N')
!
!     -- ADRESSE DU CHAMP LOCAL A MODIFIER + NBPT + NBSP
    call tecach('NOO', param, loue, 7, itab,&
                iret)
    call assert(iret.eq.0 .or. iret.eq.1)
!
!     -- SI IRET=1 : IL N'Y A RIEN A FAIRE :
    if (iret .eq. 1) then
        goto 10
    endif
!
    jtens = itab(1)
    nbpt = itab(3)
    nbsp = itab(7)
!
!     -- CAS DES ELEMENTS DE COQUE_3D :
!     -----------------------------------
    if (nomte(1:4) .eq. 'MEC3') then
!
        if (sour .eq. 'S') then
            call jevech('PCACOQU', 'L', jcara)
            epais = zr(jcara)
!         -- REMPLISSAGE DE DESR : 1090 ET 2000 :
            call jevech('PGEOMER', 'L', jgeom)
            call vdxrep(nomte, epais, zr(jgeom))
!
!         -- CALCUL DES MATRICES DE CHANGEMENT DE REPERE :
            call vdrepe(nomte, matevn, matevg)
        endif
!
!       -- MODIFICATION DU CHAMP LOCAL
        if (goun .eq. 'G') then
            call vdsiro(nbpt, nbsp, matevg, sens, goun,&
                        zr(jtens), zr(jtens))
        else
            call vdsiro(nbpt, nbsp, matevn, sens, goun,&
                        zr(jtens), zr(jtens))
        endif
!
    else
!     -- CAS DES ELEMENTS DKT, DST, Q4G  :
!     ------------------------------------
        call elref4(' ', 'RIGI', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfdx, jgano)
!
        if (sour .eq. 'S') then
            call jevech('PGEOMER', 'L', jgeom)
            if (nno .eq. 3) then
                call dxtpgl(zr(jgeom), pgl)
            else if (nno.eq.4) then
                call dxqpgl(zr(jgeom), pgl, 'S', iret)
            endif
            call jevech('PCACOQU', 'L', jcara)
            alpha = zr(jcara+1) * r8dgrd()
            beta = zr(jcara+2) * r8dgrd()
            call coqrep(pgl, alpha, beta, t2ev, t2ve,&
                        c, s)
        endif
!
        if (sens .eq. 'UI') then
            call dxsiro(nbpt*nbsp, t2ev, zr(jtens), zr(jtens))
        else
            call dxsiro(nbpt*nbsp, t2ve, zr(jtens), zr(jtens))
        endif
    endif
!
10  continue
end subroutine
