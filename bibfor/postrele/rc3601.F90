subroutine rc3601(ig, iocs, seisme, npass, ima,&
                  ipt, nbm, adrm, c, k,&
                  cara, nommat, snmax, samax, utot,&
                  sm, factus)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/rc36fp.h'
    include 'asterfort/rc36fs.h'
    include 'asterfort/rc36fu.h'
    include 'asterfort/rc36sa.h'
    include 'asterfort/rc36sn.h'
    include 'asterfort/rc36sp.h'
    include 'asterfort/rcma01.h'
    include 'asterfort/rcmo01.h'
    include 'asterfort/wkvect.h'
    integer :: ig, iocs, npass, ima, ipt, nbm, adrm(*)
    real(kind=8) :: c(*), k(*), cara(*), snmax, samax, utot, sm, factus(*)
    logical :: seisme
    character(len=8) :: nommat
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
!
!     CALCUL DES AMPLITUDES DE CONTRAINTES
!     CALCUL DU FACTEUR D'USAGE
!     ON TRAITE LES SITUATIONS COMBINABLES DANS LEUR GROUPE
!
!     Soit 2 états stabilisés I et J appartenant aux situations P et Q
!
!     on calcule le SALT(I,J) = 0,5*(EC/E)*Ke*Sn(P,Q)*Sp(I,J)
!
!     avec Sn(P,Q) = Max( Sn(I,J) )
!          Sn(I,J) = Max( Max(Sn(I,J,ThP)), Max(Sn(I,J,ThQ)) )
!
!     avec Sp(I,J) = Max( Max(Sp(I,J,ThP)), Max(Sp(I,J,ThQ)) )
!
!     ------------------------------------------------------------------
!
    integer :: nbsigr, jnsg, is1, ioc1, is2, ioc2, inds, ifm, niv, jcombi
    integer :: jpresa, jpresb, jmomea, jmomeb, jnbocc, nbth1, jth1, nbth2, jth2
    integer :: jchmat, jmsa, ndim, jnoc, nscy, ns, jmsn, nbsig2, i1, i2, indi
    integer :: jist, jnsitu, nbsitu
    real(kind=8) :: ppi, ppj, pqi, pqj, saltij, ug, sn, sp, smm, mpi(3), mpj(3)
    real(kind=8) :: mqi(3), mqj(3), mse(3), matpi(14), matpj(14), matqi(14)
    real(kind=8) :: matqj(14), matse(14)
    character(len=8) :: k8b
    character(len=24) :: momepi, momepj, momeqi, momeqj, matepi, matepj, mateqi
    character(len=24) :: mateqj
    real(kind=8) :: typeke, spmeca, spther
! DEB ------------------------------------------------------------------
    call jemarq()
!
    call infniv(ifm, niv)
!
    call jeveuo('&&RC3600.SITU_NUMERO', 'L', jnsitu)
    call jeveuo('&&RC3600.SITU_COMBINABLE', 'L', jcombi)
    call jeveuo('&&RC3600.SITU_PRES_A', 'L', jpresa)
    call jeveuo('&&RC3600.SITU_PRES_B', 'L', jpresb)
    call jeveuo('&&RC3600.SITU_MOMENT_A', 'L', jmomea)
    call jeveuo('&&RC3600.SITU_MOMENT_B', 'L', jmomeb)
    call jeveuo('&&RC3600.SITU_NB_OCCUR', 'L', jnbocc)
!
    call jeveuo('&&RC3600.MATERIAU', 'L', jchmat)
!
    call jelira('&&RC3600.SITU_PRES_A', 'LONUTI', nbsitu, k8b)
    call jelira(jexnum('&&RC3600.LES_GROUPES', ig), 'LONMAX', nbsigr, k8b)
    call jeveuo(jexnum('&&RC3600.LES_GROUPES', ig), 'L', jnsg)
    if (niv .ge. 2) then
        write (ifm,1000) ig,nbsigr
        write (ifm,1002) (zi(jnsitu+zi(jnsg+i1-1)-1),i1=1,nbsigr)
    endif
!
    if (iocs .eq. 0) then
        nbsig2 = nbsigr
    else
        nbsig2 = nbsigr - 1
    endif
    ndim = 2*nbsig2
    call wkvect('&&RC3601.NB_OCCURR', 'V V I', ndim, jnoc)
    call wkvect('&&RC3601.IMPR_SITU', 'V V I', ndim, jist)
    ndim = nbsig2*nbsig2
    call wkvect('&&RC3601.MATRICE_SN', 'V V R', ndim, jmsn)
    ndim = 4*nbsig2*nbsig2
    call wkvect('&&RC3601.MATRICE_SALT', 'V V R', ndim, jmsa)
!
    ns = 0
    if (seisme) then
        momepi = zk24(jmomea+nbsitu+iocs-1)
        call rcmo01(momepi, ima, ipt, mse)
        mse(1) = 2*mse(1)
        mse(2) = 2*mse(2)
        mse(3) = 2*mse(3)
        matepi = zk24(jchmat+2*(nbsitu+iocs)-1)
        call rcma01(matepi, ima, ipt, nbm, adrm,&
                    matse)
        ns = zi(jnbocc+2*(nbsitu+iocs)-2)
        nscy = zi(jnbocc+2*(nbsitu+iocs)-1)
    else
        mse(1) = 0.d0
        mse(2) = 0.d0
        mse(3) = 0.d0
    endif
!
!
! --- SITUATION P :
!     -------------
!
    i1 = 0
    do 20 is1 = 1, nbsigr
        ioc1 = zi(jnsg+is1-1)
        if (.not.zl(jcombi+ioc1-1)) goto 20
        if (ioc1 .gt. nbsitu) goto 20
!
        i1 = i1 + 1
        zi(jnoc-1+2* (i1-1)+1) = zi(jnbocc+2*ioc1-2)
        zi(jnoc-1+2* (i1-1)+2) = zi(jnbocc+2*ioc1-2)
        zi(jist-1+2* (i1-1)+1) = ioc1
        zi(jist-1+2* (i1-1)+2) = ioc1
!
        ppi = zr(jpresa+ioc1-1)
        momepi = zk24(jmomea+ioc1-1)
        call rcmo01(momepi, ima, ipt, mpi)
        matepi = zk24(jchmat+2*ioc1-1)
        call rcma01(matepi, ima, ipt, nbm, adrm,&
                    matpi)
        typeke = matpi(14)
!
        ppj = zr(jpresb+ioc1-1)
        momepj = zk24(jmomeb+ioc1-1)
        call rcmo01(momepj, ima, ipt, mpj)
        matepj = zk24(jchmat+2*ioc1-2)
        call rcma01(matepj, ima, ipt, nbm, adrm,&
                    matpj)
!
        call jelira(jexnum('&&RC3600.SITU_THERMIQUE', ioc1), 'LONUTI', nbth1, k8b)
        if (nbth1 .ne. 0) then
            call jeveuo(jexnum('&&RC3600.SITU_THERMIQUE', ioc1), 'L', jth1)
        else
            jth1 = 1
        endif
!
        nbth2 = 0
        jth2 = 1
        ioc2 = 0
!
        sn = 0.d0
        call rc36sn(nbm, adrm, ipt, c, cara,&
                    matpi, ppi, mpi, matpj, ppj,&
                    mpj, mse, nbth1, nbth2, ioc1,&
                    ioc2, sn)
        zr(jmsn-1+nbsig2* (i1-1)+i1) = sn
        snmax = max(snmax,sn)
!
        if (niv .ge. 2) write (ifm,1010) ioc1,sn
        inds = 4*nbsig2* (i1-1) + 4* (i1-1)
!
!
! ----- 1/ CALCUL DU SALT(I,I) A PARTIR DU SN(P,Q) ET SP(I,I)
!
        saltij = 0.d0
        zr(jmsa-1+inds+1) = saltij
!
! ----- 2/ CALCUL DU SALT(I,J) A PARTIR DU SN(P,Q) ET SP(I,J)
!
        sp = 0.d0
        spmeca = 0.d0
        spther = 0.d0
        call rc36sp(nbm, adrm, ipt, c, k,&
                    cara, matpi, ppi, mpi, matpj,&
                    ppj, mpj, mse, nbth1, nbth2,&
                    ioc1, ioc2, sp, typeke, spmeca,&
                    spther)
!
        if (niv .ge. 2) write (ifm,1032) sp
        if (typeke .gt. 0.d0) then
            if (niv .ge. 2) then
                write (ifm,1132) spmeca,spther
            endif
        endif
!
        call rc36sa(nommat, matpi, matpj, sn, sp,&
                    typeke, spmeca, spther, saltij, smm)
!
        zr(jmsa-1+inds+2) = saltij
        zr(jmsa-1+inds+3) = saltij
        if (saltij .gt. samax) then
            samax = saltij
            sm = smm
        endif
!
! ----- 3/ CALCUL DU SALT(J,J) A PARTIR DU SN(P,Q) ET SP(J,J)
!
        saltij = 0.d0
        zr(jmsa-1+inds+4) = saltij
!
! ----- SITUATION Q :
!       -------------
!
        i2 = i1
        do 10 is2 = is1 + 1, nbsigr
            ioc2 = zi(jnsg+is2-1)
            if (.not.zl(jcombi+ioc2-1)) goto 10
            if (ioc2 .gt. nbsitu) goto 10
            i2 = i2 + 1
!
            pqi = zr(jpresa+ioc2-1)
            momeqi = zk24(jmomea+ioc2-1)
            call rcmo01(momeqi, ima, ipt, mqi)
            mateqi = zk24(jchmat+2*ioc2-1)
            call rcma01(mateqi, ima, ipt, nbm, adrm,&
                        matqi)
!
            pqj = zr(jpresb+ioc2-1)
            momeqj = zk24(jmomeb+ioc2-1)
            call rcmo01(momeqj, ima, ipt, mqj)
            mateqj = zk24(jchmat+2*ioc2-2)
            call rcma01(mateqj, ima, ipt, nbm, adrm,&
                        matqj)
!
            call jelira(jexnum('&&RC3600.SITU_THERMIQUE', ioc2), 'LONUTI', nbth2, k8b)
            if (nbth2 .ne. 0) then
                call jeveuo(jexnum('&&RC3600.SITU_THERMIQUE', ioc2), 'L', jth2)
            else
                jth2 = 1
            endif
!
! ------- CALCUL DU SN(P,Q), ON A 4 COMBINAISONS
!
            sn = 0.d0
!
            call rc36sn(nbm, adrm, ipt, c, cara,&
                        matpi, ppi, mpi, matqi, pqi,&
                        mqi, mse, nbth1, nbth2, ioc1,&
                        ioc2, sn)
!
            call rc36sn(nbm, adrm, ipt, c, cara,&
                        matpi, ppi, mpi, matqj, pqj,&
                        mqj, mse, nbth1, nbth2, ioc1,&
                        ioc2, sn)
!
            call rc36sn(nbm, adrm, ipt, c, cara,&
                        matpj, ppj, mpj, matqj, pqj,&
                        mqj, mse, nbth1, nbth2, ioc1,&
                        ioc2, sn)
!
            call rc36sn(nbm, adrm, ipt, c, cara,&
                        matpj, ppj, mpj, matqi, pqi,&
                        mqi, mse, nbth1, nbth2, ioc1,&
                        ioc2, sn)
!
            zr(jmsn-1+nbsig2* (i1-1)+i2) = sn
            zr(jmsn-1+nbsig2* (i2-1)+i1) = sn
            if (niv .ge. 2) write (ifm,1020) ioc1,ioc2,sn
!
            snmax = max(snmax,sn)
            inds = 4*nbsig2* (i1-1) + 4* (i2-1)
            indi = 4*nbsig2* (i2-1) + 4* (i1-1)
!
! ------- 1/ CALCUL DU SALT(I,I) A PARTIR DU SN(P,Q) ET SP(I,I)
!
            sp = 0.d0
            spmeca = 0.d0
            spther = 0.d0
!
            call rc36sp(nbm, adrm, ipt, c, k,&
                        cara, matpi, ppi, mpi, matqi,&
                        pqi, mqi, mse, nbth1, nbth2,&
                        ioc1, ioc2, sp, typeke, spmeca,&
                        spther)
!
            if (niv .ge. 2) write (ifm,1031) sp
            if (typeke .gt. 0.d0) then
                if (niv .ge. 2) then
                    write (ifm,1131) spmeca,spther
                endif
            endif
!
            call rc36sa(nommat, matpi, matqi, sn, sp,&
                        typeke, spmeca, spther, saltij, smm)
!
            zr(jmsa-1+inds+1) = saltij
            zr(jmsa-1+indi+1) = saltij
            if (saltij .gt. samax) then
                samax = saltij
                sm = smm
            endif
!
! ------- 2/ CALCUL DU SALT(I,J) A PARTIR DU SN(P,Q) ET SP(I,J)
!
            sp = 0.d0
            spmeca = 0.d0
            spther = 0.d0
!
            call rc36sp(nbm, adrm, ipt, c, k,&
                        cara, matpi, ppi, mpi, matqj,&
                        pqj, mqj, mse, nbth1, nbth2,&
                        ioc1, ioc2, sp, typeke, spmeca,&
                        spther)
!
            if (niv .ge. 2) write (ifm,1032) sp
            if (typeke .gt. 0.d0) then
                if (niv .ge. 2) then
                    write (ifm,1132) spmeca,spther
                endif
            endif
!
            call rc36sa(nommat, matpi, matqj, sn, sp,&
                        typeke, spmeca, spther, saltij, smm)
!
            zr(jmsa-1+inds+3) = saltij
            zr(jmsa-1+indi+2) = saltij
            if (saltij .gt. samax) then
                samax = saltij
                sm = smm
            endif
!
! ------- 3/ CALCUL DU SALT(J,I) A PARTIR DU SN(P,Q) ET SP(J,I)
!
            sp = 0.d0
            spmeca = 0.d0
            spther = 0.d0
!
            call rc36sp(nbm, adrm, ipt, c, k,&
                        cara, matpj, ppj, mpj, matqi,&
                        pqi, mqi, mse, nbth1, nbth2,&
                        ioc1, ioc2, sp, typeke, spmeca,&
                        spther)
!
            if (niv .ge. 2) write (ifm,1034) sp
            if (typeke .gt. 0.d0) then
                if (niv .ge. 2) then
                    write (ifm,1134) spmeca,spther
                endif
            endif
!
            call rc36sa(nommat, matpj, matqi, sn, sp,&
                        typeke, spmeca, spther, saltij, smm)
!
            zr(jmsa-1+inds+2) = saltij
            zr(jmsa-1+indi+3) = saltij
            if (saltij .gt. samax) then
                samax = saltij
                sm = smm
            endif
!
! ------- 4/ CALCUL DU SALT(J,J) A PARTIR DU SN(P,Q) ET SP(J,J)
!
            sp = 0.d0
            spmeca = 0.d0
            spther = 0.d0
!
            call rc36sp(nbm, adrm, ipt, c, k,&
                        cara, matpj, ppj, mpj, matqj,&
                        pqj, mqj, mse, nbth1, nbth2,&
                        ioc1, ioc2, sp, typeke, spmeca,&
                        spther)
!
            if (niv .ge. 2) write (ifm,1033) sp
            if (typeke .gt. 0.d0) then
                if (niv .ge. 2) then
                    write (ifm,1133) spmeca,spther
                endif
            endif
!
            call rc36sa(nommat, matpj, matqj, sn, sp,&
                        typeke, spmeca, spther, saltij, smm)
!
            zr(jmsa-1+inds+4) = saltij
            zr(jmsa-1+indi+4) = saltij
            if (saltij .gt. samax) then
                samax = saltij
                sm = smm
            endif
!
10      continue
!
20  end do
!
! --- CALCUL DU FACTEUR D'USAGE
!
    if (seisme) then
        call rc36fs(nbsig2, zi(jnoc), zi(jist), nbsig2, zi(jnoc),&
                    zi(jist), zr(jmsa), ns, nscy, matse,&
                    mse, zr(jmsn), nommat, c, k,&
                    cara, ug)
    else
        if (npass .eq. 0) then
            call rc36fu(nbsig2, zi(jnoc), zi(jist), zr(jmsa), nommat,&
                        ug, factus)
        else
            call rc36fp(nbsig2, zi(jnoc), zi(jist), zi(jnsg), zr(jmsa),&
                        nommat, ug, factus)
        endif
    endif
!
    utot = utot + ug
!
    call jedetr('&&RC3601.MATRICE_SALT')
    call jedetr('&&RC3601.MATRICE_SN')
    call jedetr('&&RC3601.NB_OCCURR')
    call jedetr('&&RC3601.IMPR_SITU')
!
    1000 format ('=> GROUPE: ',i4,' , NOMBRE DE SITUATIONS: ',i4)
    1002 format ('=> LISTE DES SITUATIONS: ',100 (i4,1x))
    1010 format (1p,' SITUATION ',i4,' SN =',e12.5)
    1020 format (1p,' COMBINAISON DES SITUATIONS ',i4,3x,i4,'  SN =',e12.5)
    1031 format (1p,26x,'ETAT_A ETAT_A ',' SP =',e12.5)
    1032 format (1p,26x,'ETAT_B ETAT_A ',' SP =',e12.5)
    1033 format (1p,26x,'ETAT_B ETAT_B ',' SP =',e12.5)
    1034 format (1p,26x,'ETAT_A ETAT_B ',' SP =',e12.5)
!
    1131 format (1p,26x,'ETAT_A ETAT_A ',' SPMECA=',e12.5,' SPTHER=',e12.5)
    1132 format (1p,26x,'ETAT_B ETAT_A ',' SPMECA=',e12.5,' SPTHER=',e12.5)
    1133 format (1p,26x,'ETAT_B ETAT_B ',' SPMECA=',e12.5,' SPTHER=',e12.5)
    1134 format (1p,26x,'ETAT_A ETAT_B ',' SPMECA=',e12.5,' SPTHER=',e12.5)
!
    call jedema()
end subroutine
