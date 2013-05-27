subroutine jjhrsv(idts, nbval, iadmi)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: j-pierre.lefebvre at edf.fr
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
    implicit none
    include 'jeveux.h'
    include 'jeveux_private.h'
    include 'asterc/hdfcld.h'
    include 'asterc/hdfrsv.h'
    include 'asterc/hdftsd.h'
    include 'asterfort/jjalls.h'
    include 'asterfort/jjlidy.h'
    include 'asterfort/u2mess.h'
    integer :: idts, nbval, iadmi
! ----------------------------------------------------------------------
! RELIT UN SEGMENT DE VALEURS ASSOCIE A UN OBJET JEVEUX, LE TYPE INTEGER
! EST TRAITE DE FACON PARTICULIERE POUR S'AJUSTER A LA PLATE-FORME
!
! IN  IDTS   : IDENTIFICATEUR DU DATASET HDF
! IN  NBVAL  : NOMBRE DE VALEURS DU DATASET
! IN  IADMI  : ADRESSE DANS JISZON DU TABLEAU DE VALEURS LUES
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    integer :: istat
    common /istaje/  istat(4)
! ----------------------------------------------------------------------
    real(kind=8) :: svuse, smxuse
    common /statje/  svuse,smxuse
! ----------------------------------------------------------------------
    integer :: iret, jadr, kadm, nbv, k, lonoi, ltypi
    integer :: ir, kitab, iconv, iadyn
    character(len=1) :: typei
! DEB ------------------------------------------------------------------
    iconv = 0
    ltypi = 0
    nbv = 0
    iret = hdftsd(idts,typei,ltypi,nbv)
    if (iret .ne. 0) then
        call u2mess('F', 'JEVEUX_52')
    endif
    if (typei .eq. 'I') then
        iconv = 1
        if (lois .lt. ltypi) then
            lonoi = nbval*ltypi
            call jjalls(lonoi, 0, 'V', typei, lois,&
                        'INIT', zi, jadr, kadm, iadyn)
            iszon(jiszon+kadm-1) = istat(2)
            iszon(jiszon+iszon(jiszon+kadm-4)-4) = istat(4)
            svuse = svuse + (iszon(jiszon+kadm-4) - kadm + 4)
            smxuse = max(smxuse,svuse)
            ir = iszon(jiszon + kadm - 3 )
            kitab = jk1zon+(kadm-1)*lois+ir+1
            iret = hdfrsv(idts,nbv,k1zon(kitab),iconv)
            do 1 k = 1, nbv
                iszon(jiszon+iadmi-1+k)=iszon(jiszon+kadm-1+k)
 1          continue
            call jjlidy(iadyn, kadm)
        else
            ir = iszon(jiszon + iadmi - 3 )
            kitab = jk1zon+(iadmi-1)*lois+ir+1
            iret = hdfrsv(idts,nbv,k1zon(kitab),iconv)
        endif
    else
        ir = iszon(jiszon+iadmi-3)
        kitab = jk1zon+(iadmi-1)*lois+ir+1
        iret = hdfrsv(idts,nbv,k1zon(kitab),iconv)
    endif
    if (iret .ne. 0) then
        call u2mess('F', 'JEVEUX_53')
    endif
    iret = hdfcld(idts)
! FIN ------------------------------------------------------------------
end subroutine
