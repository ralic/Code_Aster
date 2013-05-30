subroutine wpnorm(norm, para, lmatr, neq, nbmode,&
                  ddlexc, vecpro, resufr, coef)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/detrsd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mcmult.h'
    include 'asterfort/mtcmbl.h'
    include 'asterfort/mtdefs.h'
    include 'asterfort/mtdscr.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: norm, para
    integer :: nbmode, neq, lmatr(*), ddlexc(*)
    complex(kind=8) :: vecpro(neq, *)
    real(kind=8) :: resufr(nbmode, *), coef(*)
!     ------------------------------------------------------------------
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
!     NORMALISATION DE VECTEURS COMPLEXES ET DE GRANDEURS MODALES
!     ------------------------------------------------------------------
! IN  NORM   : TYPE DE NORMALISATION
!          = 'AVEC_CMP'
!          = 'MASS_GENE'
!          = 'RIGI_GENE'
!          = 'EUCL'
! IN  PARA   : ON REPERCUTE LA NORMALISATION SUR LES PARAMETRES MODAUX
!          = 'OUI' DANS CE CAS ILS DOIVENT DEJA AVOIR ETE CALCULES
!          = 'NON' ON NE NORMALISE QUE LES VECTEURS PROPRES
! IN  LMTR   : DESCRIPTEUR D'UNE MATRICE
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  NBMODE : NOMBRE DE MODES
! IN  DDLEXC : TABLEAU DES DDL EXCLUS
!          = 0 SI EXCLUS
!          = 1 SI NON EXCLUS
! VAR VECPRO : TABLEAU DES VECTEURS PROPRES
! VAR RESUFR : TABLEAU DES GRANDEURS MODALES RANGEES SELON
!        'FREQ'            , 'OMEGA2'          , 'AMOR_REDUIT'     ,
!        'MASS_GENE'       , 'RIGI_GENE'       , 'AMOR_GENE'       ,
!        'MASS_EFFE_DX'    , 'MASS_EFFE_DY'    , 'MASS_EFFE_DZ'    ,
!        'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ' ,
!        'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' , 'MASS_EFFE_UN_DZ'
!     ------------------------------------------------------------------
!
!
    integer :: im, ieq, lacc1, ldynam
    character(len=1) :: typcst(2)
    character(len=19) :: matmod
    real(kind=8) :: rnorm, rx1, rx2, constr(4), fr, am, zero
    complex(kind=8) :: xx1, cmpl, xnorm, dconjg, czero
    character(len=24) :: nmatr(2), ndynam
    character(len=24) :: valk
!     ------------------------------------------------------------------
    data        typcst/'C','C'/
!     ------------------------------------------------------------------
!
    call jemarq()
    zero=0.d0
    czero=dcmplx(zero,zero)
!
    if (norm .eq. 'AVEC_CMP' .or. norm .eq. 'EUCL') then
!
!        --- NORMALISATION SUR LES DDL NON EXCLUS
        do 2 im = 1, nbmode
            rnorm = 0.0d0
            if (norm .eq. 'EUCL') then
                do 4 ieq = 1, neq
                    xx1 = vecpro(ieq,im) * ddlexc(ieq)
                    rnorm = rnorm + dble(xx1*dconjg(xx1))
 4              continue
                rnorm = sqrt( rnorm )
            else
                do 6 ieq = 1, neq
                    rx1 = abs( vecpro(ieq,im)*ddlexc(ieq) )
                    rnorm = max( rx1 , rnorm )
 6              continue
            endif
            rx1 = 1.0d0 / rnorm
            coef(im) = rx1
            do 8 ieq = 1, neq
                vecpro(ieq,im) = vecpro(ieq,im) * rx1
 8          continue
            if (para .eq. 'OUI') then
                rx2 = rx1 * rx1
                resufr(im,4) = resufr(im,4) * rx2
                resufr(im,5) = resufr(im,5) * rx2
                resufr(im,6) = resufr(im,6) * rx2
            endif
 2      continue
!
    else if (norm.eq.'MASS_GENE' .or. norm.eq.'RIGI_GENE') then
!
!        --- ON NORMALISE LA MASSE OU LA RAIDEUR GENERALISEE A 1 ---
!        --- DU PROBLEME GENERALISE ASSOCIE AU PROBLEME QUADRATIQUE ---
        matmod = zk24(zi(lmatr(1)+1))
        nmatr(1)=zk24(zi(lmatr(1)+1))
        nmatr(2)=zk24(zi(lmatr(2)+1))
        call wkvect('&&WPNORM.XXXX_GENE_2', 'V V C', neq, lacc1)
        call mtdefs('&&WPNORM.MATR.DYNAMIC', matmod, 'V', 'C')
        call mtdscr('&&WPNORM.MATR.DYNAM')
        ndynam='&&WPNORM.MATR.DYNAM'//'.&INT'
        call jeveuo(ndynam, 'E', ldynam)
        if (norm .eq. 'MASS_GENE') then
            constr(3) = 1.d0
            constr(4) = 0.d0
        else if (norm.eq.'RIGI_GENE') then
            constr(1) = -1.d0
            constr(2) = 0.d0
        endif
        do 30 im = 1, nbmode
            fr = sqrt( resufr(im,2) )
            am = resufr(im,3)
            am = -abs( am*fr ) / sqrt( 1.0d0 - am*am )
            if (norm .eq. 'MASS_GENE') then
                constr(1) = 2.d0*am
                constr(2) = 2.d0*fr
            else if (norm.eq.'RIGI_GENE') then
                cmpl = dcmplx(am,fr)
                cmpl = cmpl * cmpl
                constr(3) = dble(cmpl)
                constr(4) = dimag(cmpl)
            endif
            call mtcmbl(2, typcst, constr, nmatr, ndynam,&
                        ' ', ' ', 'ELIM=')
            call mcmult('ZERO', ldynam, vecpro(1, im), zc(lacc1), 1,&
                        .true.)
            xnorm = czero
            do 31 ieq = 1, neq
                xnorm = xnorm + vecpro(ieq,im) * zc(lacc1+ieq-1)
31          continue
            xnorm = 1.d0 / sqrt(xnorm)
            coef(im) = dble(xnorm)
            do 32 ieq = 1, neq
                vecpro(ieq,im) = vecpro(ieq,im) * xnorm
32          continue
            if (para .eq. 'OUI') then
                xnorm = xnorm * xnorm
                resufr(im,4) = resufr(im,4) * dble(xnorm)
                resufr(im,5) = resufr(im,5) * dble(xnorm)
                resufr(im,6) = resufr(im,6) * dble(xnorm)
            endif
30      continue
! --- MENAGE
        call jedetr('&&WPNORM.XXXX_GENE_2')
        call detrsd('MATR_ASSE', '&&WPNORM.MATR.DYNAMIC')
        call detrsd('MATR_ASSE', '&&WPNORM.MATR.DYNAM')
        call jedetr('&&WPNORM.MATR.DYNAM'//'.&INT')
!
    else
!
        valk = norm
        call u2mesg('F', 'ALGELINE4_77', 1, valk, 0,&
                    0, 0, 0.d0)
!
    endif
!
    call jedema()
end subroutine
