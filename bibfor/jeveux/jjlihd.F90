subroutine jjlihd(idts, nbval, lonoi, genri, typei,&
                  ltypi, ic, ido, idc, jmarq,&
                  iadmi, iadyn)
! person_in_charge: j-pierre.lefebvre at edf.fr
! aslint: disable=C1002
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
    include 'asterc/hdfrsv.h'
    include 'asterc/hdftsd.h'
    include 'asterfort/jjalls.h'
    include 'asterfort/jjecrs.h'
    include 'asterfort/jjlidy.h'
    include 'asterfort/u2mess.h'
    integer :: idts, nbval, lonoi, ltypi, ic, ido, idc, iadmi, jmarq(1)
    character(len=*) :: genri, typei
! ----------------------------------------------------------------------
! RELIT UN SEGMENT DE VALEURS DANS UN FICHIER HDF EN FONCTION DU TYPE
! ASSOCIE : SIMPLE DEBRANCHEMENT POUR FAIRE TOUJOURS PASSER UN TABLEAU
! DE TYPE INTEGER EN ARGUMENT.
! LE TYPE INTEGER EST TRAITE DE FACON PARTICULIERE POUR S'AJUSTER
! A LA PLATE-FORME
! L'ETAT ET LE STATUT DU SEGMENT DE VALEURS SONT ACTUALISES
!
! IN  IDTS  : IDENTIFICATEUR DU DATASET HDF
! IN  NBVAL  : NOMBRE DE VALEURS DU DATASET
! IN  LONOI  : LONGEUR EN OCTET DU SEGMENT DE VALEURS
! IN  GENRI  : GENRE DE L'OBJET
! IN  TYPEI  : TYPE DE L'OBJET
! IN  LTYPI  : LONGUEUR DU TYPE
! IN  ID0    : IDENTIFICATEUR DE L'OBJET JEVEUX
! IN  IDC    : IDENTIFICATEUR DE COLLECTION JEVEUX
! IN  IC     : CLASSE DE L'OBJET JEVEUX
! IN  JMARQ  : ADRESSE DE LA MARQUE JEVEUX
! OUT IADMI  : ADRESSE JEVEUX DU SEGMENT DE VALEURS
! OUT IADYN  : ADRESSE DYNAMIQUE DU SEGMENT DE VALEURS
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    integer :: istat
    common /istaje/  istat(4)
    real(kind=8) :: svuse, smxuse
    common /statje/  svuse,smxuse
    character(len=1) :: typeb
    integer :: iconv, iadyn, kdyn
    integer :: iret, jadr, kitab, nbv, ir, lon, kadm, k, lv, ltypb
    integer :: izr(1), izc(1), izl(1), izk8(1), izk16(1)
    integer :: izk24(1), izk32(1), izk80(1), izi4(1)
    equivalence    (izr,zr),(izc,zc),(izl,zl),(izk8,zk8),(izk16,zk16),&
     &               (izk24,zk24),(izk32,zk32),(izk80,zk80),(izi4,zi4)
! DEB ------------------------------------------------------------------
    iret = -1
    iconv = 0
    ltypb = 0
    typeb = ' '
    lv = 0
    nbv = nbval
    if (typei .eq. 'I') then
        call jjalls(lonoi, ic, genri, typei, ltypi,&
                    'INIT', zi, jadr, iadmi, iadyn)
    else if (typei .eq. 'S') then
        call jjalls(lonoi, ic, genri, typei, ltypi,&
                    'INIT', izi4, jadr, iadmi, iadyn)
    else if (typei .eq. 'R') then
        call jjalls(lonoi, ic, genri, typei, ltypi,&
                    'INIT', izr, jadr, iadmi, iadyn)
    else if (typei .eq. 'C') then
        call jjalls(lonoi, ic, genri, typei, ltypi,&
                    'INIT', izc, jadr, iadmi, iadyn)
        nbv = 2*nbval
    else if (typei .eq. 'K') then
        if (ltypi .eq. 8) then
            call jjalls(lonoi, ic, genri, typei, ltypi,&
                        'INIT', izk8, jadr, iadmi, iadyn)
        else if (ltypi .eq. 16) then
            call jjalls(lonoi, ic, genri, typei, ltypi,&
                        'INIT', izk16, jadr, iadmi, iadyn)
        else if (ltypi .eq. 24) then
            call jjalls(lonoi, ic, genri, typei, ltypi,&
                        'INIT', izk24, jadr, iadmi, iadyn)
        else if (ltypi .eq. 32) then
            call jjalls(lonoi, ic, genri, typei, ltypi,&
                        'INIT', izk32, jadr, iadmi, iadyn)
        else if (ltypi .eq. 80) then
            call jjalls(lonoi, ic, genri, typei, ltypi,&
                        'INIT', izk80, jadr, iadmi, iadyn)
        endif
    else if (typei .eq. 'L') then
        call jjalls(lonoi, ic, genri, typei, ltypi,&
                    'INIT', izl, jadr, iadmi, iadyn)
    endif
    call jjecrs(iadmi, ic, ido, idc, 'E',&
                jmarq)
    if (typei .eq. 'I') then
        iconv = 1
        iret = hdftsd(idts,typeb,ltypb,lv)
        if (lois .lt. ltypb) then
            lon = nbval*ltypb
            call jjalls(lon, ic, 'V', typei, lois,&
                        'INIT', zi, jadr, kadm, kdyn)
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
            call jjlidy(kdyn, kadm)
        else
            ir = iszon(jiszon + iadmi - 3 )
            kitab = jk1zon+(iadmi-1)*lois+ir+1
            iret = hdfrsv(idts,nbv,k1zon(kitab),iconv)
        endif
    else if (typei .eq. 'S') then
        ir = iszon(jiszon + iadmi - 3 )
        kitab = jk1zon+(iadmi-1)*lois+ir+1
        iret = hdfrsv(idts,nbv,k1zon(kitab),iconv)
    else
        ir = iszon(jiszon + iadmi - 3 )
        kitab = jk1zon+(iadmi-1)*lois+ir+1
        iret = hdfrsv(idts,nbv,k1zon(kitab),iconv)
    endif
    if (iret .ne. 0) then
        call u2mess('F', 'JEVEUX_51')
    endif
! FIN ------------------------------------------------------------------
end subroutine
