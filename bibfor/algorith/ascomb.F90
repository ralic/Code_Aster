subroutine ascomb(lischa, vecelz, typres, nompar, valpar,&
                  cnchar)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit      none
    include 'jeveux.h'
    include 'asterc/r8depi.h'
    include 'asterc/r8dgrd.h'
    include 'asterfort/assert.h'
    include 'asterfort/corich.h'
    include 'asterfort/fointc.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/liscpp.h'
    include 'asterfort/lislnf.h'
    include 'asterfort/lisltf.h'
    include 'asterfort/lisnnb.h'
    include 'asterfort/vtcmbl.h'
    include 'asterfort/wkvect.h'
    character(len=1) :: typres
    character(len=8) :: nompar
    real(kind=8) :: valpar
    character(len=19) :: lischa
    character(len=*) :: vecelz
    character(len=19) :: cnchar
!
! ----------------------------------------------------------------------
!
! COMBINER LES CHAM_NO
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : SD LISTE DES CHARGES
! IN  VECELE : NOM DU VECT_ELEM
! IN  TYPRES : TYPE DES VECTEURS ET DU CHAM_NO RESULTANT 'R' OU 'C'
! IN  NOMPAR : NOM DU PARAMETRE
! IN  VALPAR : VALEUR DU PARAMETRE
! OUT CNCHAR : CHAM_NO RESULTAT
!
!
!
!
    integer :: nbchar
    integer :: iret, ibid, ichar
    integer :: jcoef, jtype
    character(len=24) :: vachar
    integer :: ivec, ivecc, nbvec, jvacha
    character(len=8) :: nomfct
    character(len=8) :: k8bid
    character(len=24) :: chamno
    real(kind=8) :: valres, valre, valim
    complex(kind=8) :: calpha
    real(kind=8) :: phase, omega, dgrd
    integer :: npuis
    character(len=19) :: vecele
    character(len=16) :: typfct
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- VERIFICATION DU VACHAR
!
    vecele = vecelz
    vachar = vecele(1:19)//'.CHNO'
    call jeexin(vachar, iret)
    call assert(iret.ne.0)
    call jelira(vachar, 'LONMAX', nbvec, k8bid)
    call assert(nbvec.ne.0)
    call jeveuo(vachar, 'L', jvacha)
    call assert(typres.eq.'R'.or.typres.eq.'C')
    call lisnnb(lischa, nbchar)
    dgrd = r8dgrd()
!
! --- CALCUL DES COEFFICIENTS - CAS REEL
!
    if (typres .eq. 'R') then
        call wkvect('&&ASCOMB.COEF', 'V V R8', nbvec, jcoef)
        call wkvect('&&ASCOMB.TYPE', 'V V K8', nbvec, jtype)
        do 10 ivec = 1, nbvec
!
! ------- NOM DU CHAMNO
!
            chamno = zk24(jvacha+ivec-1)
!
! ------- NUMERO DE LA CHARGE
!
            call corich('L', chamno, ibid, ichar)
            call assert((ichar.ne.0).and.(ichar.ge.-2))
!
! ------- FONCTION MULTIPLICATRICE
!
            if (ichar .gt. 0) then
                call lislnf(lischa, ichar, nomfct)
                call lisltf(lischa, ichar, typfct)
            endif
!
! ------- VALEUR DU COEFFICIENT
!
            if (ichar .eq. -1) then
                valres = 1.d0
            else if (ichar.eq.-2) then
                valres = 0.d0
            else if (ichar.gt.0) then
                valres = 1.d0
                if (nomfct .ne. ' ') then
                    call assert(typfct(7:10).eq.'REEL')
                    call fointe('F', nomfct, 1, nompar, valpar,&
                                valres, iret)
                endif
            else
                call assert(.false.)
            endif
!
            zr(jcoef+ivec-1) = valres
            zk8(jtype+ivec-1) = 'R'
10      continue
    endif
!
! --- CALCUL DES COEFFICIENTS - CAS COMPLEXE
!
    if (typres .eq. 'C') then
        omega = r8depi()*valpar
        call wkvect('&&ASCOMB.COEF', 'V V R8', 2*nbvec, jcoef)
        call wkvect('&&ASCOMB.TYPE', 'V V K8', nbvec, jtype)
        ivecc = 0
        do 20 ivec = 1, nbvec
!
! ------- NOM DU CHAMNO
!
            chamno = zk24(jvacha+ivec-1)
!
! ------- NUMERO DE LA CHARGE
!
            call corich('L', chamno, ibid, ichar)
            call assert((ichar.ne.0).and.(ichar.ge.-2))
!
! ------- FONCTION MULTIPLICATRICE
!
            if (ichar .gt. 0) then
                call lislnf(lischa, ichar, nomfct)
                call lisltf(lischa, ichar, typfct)
            endif
!
! ------- MULTIPLICATEUR COMPLEXE
!
            if (ichar .gt. 0) then
                call liscpp(lischa, ichar, phase, npuis)
            endif
!
! ------- VALEUR DU COEFFICIENT
!
            if (ichar .eq. -1) then
                valre = 1.d0
                valim = 0.d0
                calpha = 1.d0
            else if (ichar.eq.-2) then
                valre = 0.d0
                valim = 0.d0
                calpha = 1.d0
            else if (ichar.gt.0) then
                valre = 1.d0
                valim = 0.d0
                calpha = exp(dcmplx(0.d0,phase*dgrd))
                if (npuis .ne. 0) calpha = calpha*omega**npuis
                if (nomfct .ne. ' ') then
                    if (typfct(7:10) .eq. 'REEL') then
                        call fointe('F', nomfct, 1, nompar, valpar,&
                                    valre, iret)
                        valim = 0.d0
                    else if (typfct(7:10).eq.'COMP') then
                        call fointc('F', nomfct, 1, nompar, valpar,&
                                    valre, valim, iret)
                    else
                        call assert(.false.)
                    endif
                endif
            else
                call assert(.false.)
            endif
!
            zk8(jtype+ivec-1) = 'C'
            ivecc = ivecc + 1
            zr(jcoef+ivecc-1) = valre*dble(calpha)-valim*dimag(calpha)
            ivecc = ivecc + 1
            zr(jcoef+ivecc-1) = valim*dble(calpha)+valre*dimag(calpha)
!
20      continue
    endif
!
! --- COMBINAISON LINEAIRE DES CHAM_NO
!
    call vtcmbl(nbvec, zk8(jtype), zr(jcoef), zk8(jtype), zk24(jvacha),&
                zk8(jtype), cnchar)
!
    call jedetr('&&ASCOMB.COEF')
    call jedetr('&&ASCOMB.TYPE')
!
    call jedema()
end subroutine
