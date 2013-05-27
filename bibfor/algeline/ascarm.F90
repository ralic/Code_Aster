subroutine ascarm(nomsy, monoap, nbsup, nsupp, neq,&
                  nbmode, vecmod, parmod, id, reasup,&
                  spectr, repmod, corfre, amort, muapde,&
                  tcosup, im, nbdis)
    implicit  none
    integer :: nbsup, nsupp(*), neq, nbmode, id, tcosup(nbsup, *), im, nbdis(*)
    real(kind=8) :: vecmod(neq, *), spectr(*), amort(*)
    real(kind=8) :: parmod(nbmode, *), repmod(nbsup, neq, *)
    real(kind=8) :: reasup(nbsup, nbmode, *)
    character(len=16) :: nomsy
    logical :: monoap, corfre, muapde
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     COMMANDE : COMB_SISM_MODAL
!        CALCUL DE LA REPONSE POUR CHAQUE MODE
!     ------------------------------------------------------------------
! IN  : NOMSY  : OPTION DE CALCUL
! IN  : MONOAP : =.TRUE. , CAS DU MONO-APPUI
! IN  : NBSUP  : NOMBRE DE SUPPORT
! IN  : NSUPP  : MAX DU NOMBRE DE SUPPORT PAR DIRECTION
! IN  : NEQ    : NOMBRE D'EQUATIONS
! IN  : NBMODE : NOMBRE DE MODES
! IN  : VECMOD : VECTEUR DES MODES
! IN  : PARMOD : VECTEUR DES PARAMETRES MODAUX
! IN  : ID     : DIRECTION
! IN  : REASUP : TABLEAU DES REACTIONS MODALES AUX SUPPORTS
! IN  : SPECTR : TABLEAU DES VALEURS DU SPECTRE
! OUT : REPMOD : VECTEUR DES REPONSES MODALES
! IN  : CORFRE : = .TRUE.  , CORRECTION DES FREQUENCES
! IN  : AMORT  : VECTEUR DES AMORTISSEMENTS MODAUX
! IN  : NBDIS  : APPARTENANCE DES SUPPORTS AUX INTRAGROUPES
!     ------------------------------------------------------------------
    integer :: in, is, ind, ioc
    real(kind=8) :: un, xamo, omega, omega2, xxm, xxx, yyy
!     ------------------------------------------------------------------
!
    un = 1.d0
!
!     --- CAS DU MONO-APPUI ---
!
    if (monoap) then
        omega = sqrt(parmod(im,1))
        xamo = amort(im)
        if (corfre) omega = omega * sqrt( un - xamo*xamo )
        omega2 = omega * omega
        ind = id + 3*(im-1)
        xxx = ( parmod(im,2+id) * spectr(ind) ) / omega2
        if (nomsy(1:4) .eq. 'VITE') xxx = xxx * omega
        if (nomsy(1:4) .eq. 'ACCE') xxx = xxx * omega2
        do 12 in = 1, neq
            repmod(nbsup,in,id) = xxx * vecmod(in,im)
12      continue
!
!     --- CAS DU MULTI-APPUI ---
!
    else
        do 13 is = 1, nbsup
            do 17 in = 1, neq
                repmod(is,in,id) = 0.d0
17          continue
13      continue
        omega = sqrt(parmod(im,1))
        xamo = amort(im)
        if (corfre) omega = omega * sqrt( un - xamo*xamo )
        omega2 = omega * omega
        xxm = -un / ( parmod(im,2) * omega2 * omega2 )
        if (nomsy(1:4) .eq. 'VITE') xxm = xxm * omega
        if (nomsy(1:4) .eq. 'ACCE') xxm = xxm * omega2
        do 22 is = 1, nsupp(id)
            ind = id + 3*(im-1) + 3*nbmode*(is-1)
            xxx = reasup(is,im,id) * xxm * spectr(ind)
            do 24 in = 1, neq
                ioc = nbdis(is)
                repmod(ioc,in,id) = repmod(ioc,in,id)+ xxx * vecmod( in,im)
24          continue
22      continue
    endif
!
! --- CAS CORRELE : ON RECOMBINE LES SUPPORTS
!
    if (.not.muapde) then
        do 100 in = 1, neq
            yyy=0.d0
            do 110 is = 1, nsupp(id)
                if (tcosup(is,id) .eq. 1) then
!              --- COMBINAISON QUADRATIQUE ---
                    xxx = repmod(is,in,id)
                    yyy=yyy+sqrt(xxx*xxx)
!
                else if (tcosup(is,id).eq.2) then
!              --- COMBINAISON LINEAIRE ---
                    xxx = repmod(is,in,id)
                    yyy=yyy+xxx
!
                else
!              --- COMBINAISON VALEUR ABSOLUE ---
                    xxx = abs( repmod(is,in,id) )
                    yyy=yyy+xxx
                endif
110          continue
            repmod(1,in,id) = yyy
100      continue
    endif
!
end subroutine
