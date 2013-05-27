subroutine mefrep(nbz, nbmod, nbcyl, nbgrp, numgrp,&
                  z, freq0, rho, visc, rint,&
                  phix, phiy, dcent, matma)
    implicit none
    include 'jeveux.h'
    include 'asterc/r8pi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/mefin1.h'
    include 'asterfort/wkvect.h'
    integer :: nbz, nbmod, nbcyl, nbgrp, numgrp(*)
    real(kind=8) :: z(*), freq0(*), rho(*), visc(*), rint(*), phix(*), phiy(*)
    real(kind=8) :: dcent(*), matma(*)
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ----------------------------------------------------------------------
!     CALCUL DE L'AMORTISSEMENT AJOUTE DU AU FLUIDE AU REPOS
!     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST
! ----------------------------------------------------------------------
!     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
!     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
!     DE TUBES SOUS ECOULEMENT AXIAL"
!-----------------------------------------------------------------------
! IN  : NBZ    : NOMBRE DE POINTS DE DISCRETISATION
! IN  : NBMOD  : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
! IN  : NBCYL  : NOMBRE DE CYLINDRES DU FAISCEAU
! IN  : NBGRP  : NOMBRE DE GROUPES D'EQUIVALENCE
! IN  : NUMGRP : INDICES DES GROUPES D'EQUIVALENCE
! IN  : Z      : COORDONNEES 'Z' DES POINTS DE DISCRETISATION DANS LE
!                REPERE AXIAL
! IN  : FREQ0  : FREQUENCES MODALES EN FLUIDE AU REPOS AVANT PRISE EN
!                COMPTE DE L'AMORTISSEMENT FLUIDE AU REPOS
! IN  : RHO    : MASSE VOLUMIQUE DU FLUIDE AUX POINTS DE DISCRETISATION
! IN  : VISC   : VISCOSITE DU FLUIDE AUX POINTS DE DISCRETISATION
! IN  : RINT   : RAYONS DES CYLINDRES
! IN  : PHIX   : DEFORMEES MODALES INTERPOLEES DANS LE PLAN AXIAL
! IN  : PHIY   : DEFORMEES MODALES INTERPOLEES DANS LE PLAN AXIAL
! IN  : VNXX   : COEFFICIENT INTERVENANT DANS L EXPRESSION DES EFFORTS
!                VISQUEUX NORMAUX SUIVANT XX
! IN  : VNXY   : COEFFICIENT INTERVENANT DANS L EXPRESSION DES EFFORTS
!                VISQUEUX NORMAUX SUIVANT XY
! IN  : VNYX   : COEFFICIENT INTERVENANT DANS L EXPRESSION DES EFFORTS
!                VISQUEUX NORMAUX SUIVANT YX
! IN  : VNYY   : COEFFICIENT INTERVENANT DANS L EXPRESSION DES EFFORTS
!                VISQUEUX NORMAUX SUIVANT YY
! IN/ : MATMA  : VECTEUR CONTENANT LES MATRICES MODALES, MASSE,RIGIDITE,
! OUT            AMORTISSEMENT - EN SORTIE LES AMORTISSEMENTS MODAUX
!                SONT PERTURBES PAR LA CONTRIBUTION DU FLUIDE AU REPOS
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: imod, igrp, jgrp, icyl, ncyl
    real(kind=8) :: amor, rayo
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ifct, ippxx, ippxy, ippyx, ippyy, ivnxx, ivnxy
    integer :: ivnyx, ivnyy, nz
    real(kind=8) :: pi
!-----------------------------------------------------------------------
    call jemarq()
!
!
! --- DECALAGES DES TABLEAUX DE COEFFICIENTS ET DES MATRICES EN AIR
! --- DANS LE VECTEUR DCENT
!
    ippxx = nbcyl + nbcyl + nbcyl*nbcyl + nbcyl*nbcyl
    ippxy = ippxx + nbcyl*nbgrp
    ippyx = ippxy + nbcyl*nbgrp
    ippyy = ippyx + nbcyl*nbgrp
    ivnxx = ippyy + nbcyl*nbgrp
    ivnxy = ivnxx + nbcyl*nbgrp
    ivnyx = ivnxy + nbcyl*nbgrp
    ivnyy = ivnyx + nbcyl*nbgrp
!
    call wkvect('&&MEFREP.TEMP.FCT', 'V V R', nbz, ifct)
!
    pi = r8pi()
!
    do 1 nz = 1, nbz
        zr(ifct+nz-1) = rho(nz)*sqrt(visc(nz))
 1  end do
!
!
    do 2 imod = 1, nbmod
        amor = 0.d0
        do 21 igrp = 1, nbgrp
            do 211 icyl = 1, nbcyl
                if (numgrp(icyl) .eq. igrp) then
                    rayo = rint(icyl)
                endif
211          continue
            do 212 jgrp = 1, nbgrp
                ncyl = 0
                if (igrp .eq. jgrp) then
                    do 2121 icyl = 1, nbcyl
                        if (numgrp(icyl) .eq. igrp) then
                            ncyl = ncyl-1
                        endif
2121                  continue
                endif
!
                amor = amor-rayo* (&
                       (&
                       dcent(ivnxx+nbcyl*(jgrp-1)+igrp) + dble(ncyl)) * mefin1(nbz, nbgrp, imod,&
                       igrp, imod, jgrp, z, phix, phix,&
                       zr(ifct)) + dcent(ivnxy+nbcyl*(jgrp-1)+ igrp) * mefin1(nbz, nbgrp, imod,&
                       igrp, imod, jgrp, z, phix, phiy,&
                       zr(ifct)) + dcent(ivnyx+nbcyl*(jgrp-1)+igrp) * mefin1(nbz, nbgrp, imod,&
                       igrp, imod, jgrp, z, phiy, phix,&
                       zr( ifct)) + ( dcent(ivnyy+nbcyl*(jgrp-1)+igrp) + dble( ncyl) ) * mefin1(n&
                       &bz,&
                       nbgrp, imod, igrp, imod, jgrp, z, phiy, phiy, zr(ifct)&
                       )&
                       )
212          continue
21      continue
        amor = 4.d0*pi*sqrt(pi*freq0(imod))*amor
        matma(2*nbmod+imod) = matma(2*nbmod+imod)+amor
!
 2  end do
!
! --- MENAGE
    call jedetr('&&MEFREP.TEMP.FCT')
    call jedema()
end subroutine
