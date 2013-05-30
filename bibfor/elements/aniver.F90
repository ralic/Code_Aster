subroutine aniver(mater)
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
!.======================================================================
    implicit none
!
!      ANIVER --   CALCUL DES VALEURS PROPRES DE LA MATRICE
!                  HOOKE POUR S'ASSURER QUE CELLE EST BIEN
!                  DEFINIE POSITIVE DANS LE CAS DE L'ORTHOTROPIE
!                  OU DE L'ISOTROPIE TRANSVERSE
!
!   ARGUMENT        E/S  TYPE         ROLE
!    MATER          IN     K8       MATERIAU
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
    include 'jeveux.h'
    include 'asterc/indik8.h'
    include 'asterfort/dortvp.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mess.h'
    character(len=8) :: mater
! -----  VARIABLES LOCALES
    character(len=2) :: m2blan
    character(len=2) :: k8bid
    character(len=16) :: nomrc
    character(len=19) :: noobrc
!
    real(kind=8) :: dorth(6, 6)
    real(kind=8) :: nu12, nu21, nu13, nu31, nu23, nu32
!
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! ---- INITIALISATIONS
!      ---------------
!-----------------------------------------------------------------------
    integer :: i, iel, ien, iet, igln, iglt, igtn
    integer :: inuln, inult, inutn, j, jnomrc, jtypfo, jvalrk
    integer :: jvalrm, k, nbcrme, nbr, ndim
    real(kind=8) :: c1, delta, deux, e1, e2, e3, g12
    real(kind=8) :: g13, g23, un, undemi, zero
!-----------------------------------------------------------------------
    zero = 0.0d0
    undemi = 0.5d0
    un = 1.0d0
    deux = 2.0d0
!
    m2blan = ' '
    k8bid = ' '
!
    do 10 i = 1, 6
        do 10 j = 1, 6
            dorth(i,j) = zero
10      continue
!
    e1 = zero
    e2 = zero
    e3 = zero
    g12 = zero
    g23 = zero
    g13 = zero
    nu12 = zero
    nu23 = zero
    nu13 = zero
!
! --- RECUPERATION DU NOMBRE DE RELATIONS DE COMPORTEMENT :
!     ---------------------------------------------------
    call jelira(mater//'.MATERIAU.NOMRC', 'LONMAX', nbcrme, k8bid)
!
! --- RECUPERATION DU TABLEAU DES RELATIONS DE COMPORTEMENT :
!     -----------------------------------------------------
    call jeveuo(mater//'.MATERIAU.NOMRC', 'L', jnomrc)
!
! --- RECUPERATION DE L'INFORMATION MATERIAU FONCTION OU NON :
!     ------------------------------------------------------
    call jeveuo('&&OP0005.TYPFON', 'L', jtypfo)
!
! --- BOUCLE SUR LES RELATIONS DE COMPORTEMENT :
!     ----------------------------------------
    do 20 k = 1, nbcrme
        nomrc = zk16(jnomrc+k-1)
        noobrc = mater//'.'//zk16(jnomrc+k-1)(1:10)
!
! --- SI LE MATERIAU N'EST PAS UNE FONCTION :
!     -------------------------------------
        if (.not.zl(jtypfo+k-1)) then
!
! ---   ON NE TRAITE QUE LES CAS ISOTROPE-TRANSVERSE ET ORTHOTROPE :
!       ----------------------------------------------------------
            if (nomrc .eq. 'ELAS_ISTR' .or. nomrc .eq. 'ELAS_ORTH') then
!
! ---     RECUPERATION DU NOM DES COMPOSANTES ET DES VALEURS
! ---     DEFINISSANT LE MATERIAU :
!         -----------------------
                call jeveuo(noobrc//'.VALR', 'L', jvalrm)
                call jeveuo(noobrc//'.VALK', 'L', jvalrk)
!
! ---     LONGUEUR DU TABLEAU DES COMPOSANTES :
!         -----------------------------------
                call jelira(noobrc//'.VALR', 'LONUTI', nbr, k8bid)
!
! ---     RECUPERATION DES INDICES DES COMPOSANTES RELATIVES
! ---     A L'ORTHOTROPIE ET A L'ISOTROPIE TRANSVERSE DANS
! ---     LE TABLEAU DU NOM DES COMPOSANTES :
!         ---------------------------------
                iel = indik8(zk8(jvalrk),'E_L',1,nbr)
                iet = indik8(zk8(jvalrk),'E_T',1,nbr)
                ien = indik8(zk8(jvalrk),'E_N',1,nbr)
!
                iglt = indik8(zk8(jvalrk),'G_LT',1,nbr)
                igtn = indik8(zk8(jvalrk),'G_TN',1,nbr)
                igln = indik8(zk8(jvalrk),'G_LN',1,nbr)
!
                inult = indik8(zk8(jvalrk),'NU_LT',1,nbr)
                inutn = indik8(zk8(jvalrk),'NU_TN',1,nbr)
                inuln = indik8(zk8(jvalrk),'NU_LN',1,nbr)
!
! ---     RECUPERATION DES COMPOSANTES RELATIVES A L'ORTHOTROPIE
! ---     ET A L'ISOTROPIE TRANSVERSE :
!         ---------------------------
                if (iel .ne. 0) e1 = zr(jvalrm+iel-1)
                if (iet .ne. 0) e2 = zr(jvalrm+iet-1)
                if (ien .ne. 0) e3 = zr(jvalrm+ien-1)
!
                if (iglt .ne. 0) g12 = zr(jvalrm+iglt-1)
                if (igtn .ne. 0) g23 = zr(jvalrm+igtn-1)
                if (igln .ne. 0) g13 = zr(jvalrm+igln-1)
!
                if (inult .ne. 0) nu12 = zr(jvalrm+inult-1)
                if (inutn .ne. 0) nu23 = zr(jvalrm+inutn-1)
                if (inuln .ne. 0) nu13 = zr(jvalrm+inuln-1)
!
! ---     TRAITEMENT DU CAS DE L'ISOTROPIE TRANSVERSE :
!         -------------------------------------------
                if (nomrc .eq. 'ELAS_ISTR') then
!
! ---       SI G13 = 0 , ON PEUT SUPPOSER QUE L'ON EST EN 2D
! ---       ON NE TRAITE QUE LE CAS DEFORMATIONS PLANES OU
! ---       AXISYMETRIQUE CAR LE CAS CONTRAINTES PLANES REVIENT
! ---       A L'ELASTICITE ISOTROPE :
!           -----------------------
                    if (igln .eq. 0) then
                        ndim = 2
                        if (ien .eq. 0) goto 20
                        if (e3 .eq. zero) goto 20
!
                        c1 = e1/(un+nu12)
                        delta = un - nu12 - deux*nu13*nu13*e1/e3
!
                        dorth(1,1) = c1*(un - nu13*nu13*e1/e3)/delta
                        dorth(1,2) = c1*((un - nu13*nu13*e1/e3)/delta - un)
                        dorth(1,3) = e1*nu13/delta
                        dorth(2,1) = dorth(1,2)
                        dorth(2,2) = dorth(1,1)
                        dorth(2,3) = dorth(1,3)
                        dorth(3,1) = dorth(1,3)
                        dorth(3,2) = dorth(2,3)
                        dorth(3,3) = e3*(un - nu12)/delta
                        dorth(4,4) = undemi*c1
!
! ---         CALCUL DES VALEURS PROPRES DE LA MATRICE DORTH :
!             ----------------------------------------------
                        call dortvp(ndim, nomrc, dorth, 'DP')
!
! ---       TRAITEMENT DU CAS 3D :
!           --------------------
                    else if (igln.ne.0) then
                        ndim = 3
                        if (ien .eq. 0) goto 20
                        if (e3 .eq. zero) goto 20
!
                        c1 = e1/(un+nu12)
                        delta = un - nu12 - deux*nu13*nu13*e1/e3
!
                        dorth(1,1) = c1*(un - nu13*nu13*e1/e3)/delta
                        dorth(1,2) = c1*((un - nu13*nu13*e1/e3)/delta - un)
                        dorth(1,3) = e1*nu13/delta
                        dorth(2,1) = dorth(1,2)
                        dorth(2,2) = dorth(1,1)
                        dorth(2,3) = dorth(1,3)
                        dorth(3,1) = dorth(1,3)
                        dorth(3,2) = dorth(2,3)
                        dorth(3,3) = e3*(un - nu12)/delta
                        dorth(4,4) = undemi*c1
                        dorth(5,5) = g13
                        dorth(6,6) = dorth(5,5)
!
! ---         CALCUL DES VALEURS PROPRES DE LA MATRICE DORTH :
!             ----------------------------------------------
                        call dortvp(ndim, nomrc, dorth, m2blan)
!
                    endif
!
! ---     TRAITEMENT DU CAS DE L'ORTHOTROPIE :
!         ----------------------------------
                else if (nomrc.eq.'ELAS_ORTH') then
!
! ---       SI G13 = 0 , ON PEUT SUPPOSER QUE L'ON EST EN 2D :
!           ------------------------------------------------
                    if (igln .eq. 0) then
                        ndim = 2
                        if (iet .eq. 0) goto 20
                        if (e2 .eq. zero) goto 20
                        if (e3 .eq. zero) goto 20
                        if (ien .eq. 0) then
                            call u2mess('A', 'ELEMENTS_9')
                            goto 100
                        endif
!
! ---         TRAITEMENT DES CAS DES DEFORMATIONS PLANES
! ---         ET DE L'AXISYMETRIE :
!             -------------------
                        nu21 = e2*nu12/e1
                        nu31 = e1*nu13/e3
                        nu32 = e2*nu23/e3
                        delta = un-nu23*nu32-nu31*nu13-nu21*nu12 -deux*nu23*nu31*nu21
!
                        dorth(1,1) = (un - nu23*nu32)*e1/delta
                        dorth(1,2) = (nu21 + nu13*nu32)*e1/delta
                        dorth(1,3) = (nu13 + nu21*nu23)*e1/delta
                        dorth(2,2) = (un - nu13*nu31)*e2/delta
                        dorth(2,3) = (nu23 + nu13*nu12)*e2/delta
                        dorth(3,3) = (un - nu21*nu12)*e3/delta
                        dorth(2,1) = dorth(1,2)
                        dorth(3,1) = dorth(1,3)
                        dorth(3,2) = dorth(2,3)
!
                        dorth(4,4) = g12
!
! ---         CALCUL DES VALEURS PROPRES DE LA MATRICE DORTH :
!             ----------------------------------------------
                        call dortvp(ndim, nomrc, dorth, 'DP')
!
! ---         TRAITEMENT DU CAS DES CONTRAINTES PLANES :
!             ----------------------------------------
100                      continue
!
                        do 30 i = 1, 6
                            do 30 j = 1, 6
                                dorth(i,j) = zero
30                          continue
!
                        nu21 = e2*nu12/e1
                        delta = un-nu12*nu21
!
                        dorth(1,1) = e1/delta
                        dorth(1,2) = nu12*e2/delta
                        dorth(2,2) = e2/delta
                        dorth(2,1) = dorth(1,2)
!
                        dorth(4,4) = g12
! ---         CALCUL DES VALEURS PROPRES DE LA MATRICE DORTH :
!             ----------------------------------------------
                        call dortvp(ndim, nomrc, dorth, 'CP')
!
! ---       TRAITEMENT DU CAS 3D :
!           --------------------
                    else if (igln.ne.0) then
                        ndim = 3
                        if (iet .eq. 0) goto 20
                        if (e2 .eq. zero) goto 20
                        if (e3 .eq. zero) goto 20
                        if (ien .eq. 0) then
                            ndim = 2
                            do 31 i = 1, 6
                                do 31 j = 1, 6
                                    dorth(i,j) = zero
31                              continue
!
                            nu21 = e2*nu12/e1
                            delta = un-nu12*nu21
!
                            dorth(1,1) = e1/delta
                            dorth(1,2) = nu12*e2/delta
                            dorth(2,2) = e2/delta
                            dorth(2,1) = dorth(1,2)
                            dorth(4,4) = g12
! ---           CALCUL DES VALEURS PROPRES DE LA MATRICE DORTH :
                            call dortvp(ndim, nomrc, dorth, 'CP')
                        endif
!
                        nu21 = e2*nu12/e1
                        nu31 = e1*nu13/e3
                        nu32 = e2*nu23/e3
                        delta = un-nu23*nu32-nu31*nu13-nu21*nu12 -deux*nu23*nu31*nu21
!
                        dorth(1,1) = (un - nu23*nu32)*e1/delta
                        dorth(1,2) = (nu21 + nu13*nu32)*e1/delta
                        dorth(1,3) = (nu13 + nu21*nu23)*e1/delta
                        dorth(2,2) = (un - nu13*nu31)*e2/delta
                        dorth(2,3) = (nu23 + nu13*nu12)*e2/delta
                        dorth(3,3) = (un - nu21*nu12)*e3/delta
                        dorth(2,1) = dorth(1,2)
                        dorth(3,1) = dorth(1,3)
                        dorth(3,2) = dorth(2,3)
!
                        dorth(4,4) = g12
                        dorth(5,5) = g13
                        dorth(6,6) = g23
!
! ---         CALCUL DES VALEURS PROPRES DE LA MATRICE DORTH :
!             ----------------------------------------------
                        call dortvp(ndim, nomrc, dorth, m2blan)
!
                    endif
!
                endif
!
            endif
!
        endif
!
20  end do
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine
