subroutine dimax2(jdom, nbpt, cuon, cvon, rayon,&
                  cupn, cvpn, iret)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    integer :: jdom, nbpt, iret
    real(kind=8) :: cuon, cvon, rayon, cupn, cvpn
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
! person_in_charge: jean.angles at edf.fr
! ---------------------------------------------------------------------
! BUT: PARMI UNE LISTE DE POINTS, DETERMINER LE POINT QUI EST LE PLUS
!      ELOIGNE D'UN POINT DONNE.
! ---------------------------------------------------------------------
! ARGUMENTS :
!     JDOM    : IN  : ADRESSE DU VECTEUR CONTENANT LES POINTS DU
!                     DOMAINE PARMI LESQUELS, NOUS CHERCHONS S'IL
!                     EXISTE UN POINT QUI SOIT PLUS ELOIGNE DU CENTRE
!                     QUE LE RAYON PRECEDEMMENT CALCULE.
!     NBPT    : IN  : NOMBRE DE POINTS DANS LE DOMAINE.
!     CUON    : IN  : COMPOSANTE U DU CENTRE "On".
!     CVON    : IN  : COMPOSANTE V DU CENTRE "On".
!     RAYON   : IN  : VALEUR DU RAYON CIRCONSCRIT AVANT RECHERCHE D'UN
!                     POINT PLUS ELOIGNE DU CENTRE "On".
!     CUPN    : OUT : COMPOSANTE U DU POINT LE PLUS ELOIGNE S'IL EN
!                     EXISTE UN.
!     CVPN    : OUT : COMPOSANTE V DU POINT LE PLUS ELOIGNE S'IL EN
!                     EXISTE UN.
!     IRET    : OUT : INDIQUE S'IL EXISTE UN POINT PLUS ELOIGNE
!                     DU CENTRE "On" QUE DE LA VALEUR DU RAYON
!                     CIRCONSCRIT PRECEDENT.
!                     IRET = 0 => IL N'Y A PAS DE POINT PLUS ELOIGNE;
!                     IRET = 1 => IL Y A AUMOINS UN POINT PLUS ELOIGNE.
!     -----------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: i
    real(kind=8) :: cui, cvi, ray0, dist, epsilo
!     ------------------------------------------------------------------
!
!234567                                                              012
    call jemarq()
!
    iret = 0
    cupn = 0.0d0
    cvpn = 0.0d0
    ray0 = rayon
    epsilo = 1.0d-4
!
    do 10 i = 1, nbpt
        cui = zr(jdom + (i-1)*2)
        cvi = zr(jdom + (i-1)*2 + 1)
        dist = sqrt((cuon - cui)**2 + (cvon - cvi)**2)
!
        if (dist .gt. (ray0*(1.0d0+epsilo))) then
            ray0 = dist
            cupn = cui
            cvpn = cvi
            iret = 1
        endif
!
10  end do
!
    call jedema()
end subroutine
