subroutine exchno(imodat, iparg)
    implicit none
!
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
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/trigd.h'
    include 'asterfort/u2mess.h'
    integer :: imodat, iparg
! ----------------------------------------------------------------------
!     ENTREES:
!        IMODAT  : INDICE DANS LA COLLECTION MODELOC
!        IGR    : NUMERO DU GREL A TRAITER.
!     SORTIES:
!       ECRITURE DANS LE CHAMP LOCAL
! ----------------------------------------------------------------------
    integer :: igd, nec, ncmpmx, iachin, iachlo, iichin, ianueq, lprno
    integer :: ilchlo, itypgd
    common /caii01/igd,nec,ncmpmx,iachin,iachlo,iichin,ianueq,lprno,&
     &       ilchlo,itypgd
    character(len=8) :: typegd
    common /cakk02/typegd
    integer :: iaoptt, lgco, iaopmo, ilopmo, iaopno, ilopno, iaopds, iaoppa
    integer :: npario, nparin, iamloc, ilmloc, iadsgd
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
    integer :: iamaco, ilmaco, iamsco, ilmsco, ialiel, illiel
    common /caii03/iamaco,ilmaco,iamsco,ilmsco,ialiel,illiel
    integer :: iachii, iachik, iachix
    common /caii04/iachii,iachik,iachix
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
!
!     FONCTIONS EXTERNES:
!     -------------------
    integer :: numail, numglm, numgls
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: ima, ino, nno, long, nugl, num, jparal, iret, iel
    integer :: desc, prno1, prno2, modloc, ityplo
    integer :: deb1, deb2, idg1, idg2, nbpt, nbpt2, lgcata, ncmp
    integer :: iaux1, k, iec, debugr
    logical :: lparal
!
    logical :: diff, moyenn
!
!     -- FONCTIONS FORMULES :
!     NUMAIL(IGR,IEL)=NUMERO DE LA MAILLE ASSOCIEE A L'ELEMENT IEL
    numail(igr,iel)=zi(ialiel-1+zi(illiel+igr-1)+iel-1)
!     NUMGLM(IMA,INO)=NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
!                     IMA ETANT UNE MAILLE DU MAILLAGE.
    numglm(ima,ino)=zi(iamaco-1+zi(ilmaco+ima-1)+ino-1)
!     NUMGLS(IMA,INO)=NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
!                     IMA ETANT UNE MAILLE SUPPLEMENTAIRE DU LIGREL
    numgls(ima,ino)=zi(iamsco-1+zi(ilmsco+ima-1)+ino-1)
! DEB-------------------------------------------------------------------
!
!     PARALLELE OR NOT ?
!     -------------------------
    call jeexin('&CALCUL.PARALLELE', iret)
    if (iret .ne. 0) then
        lparal=.true.
        call jeveuo('&CALCUL.PARALLELE', 'L', jparal)
    else
        lparal=.false.
    endif
!
    desc=zi(iachii-1+11*(iichin-1)+4)
    num=zi(desc-1+2)
    modloc=iamloc-1+zi(ilmloc-1+imodat)
    ityplo=zi(modloc-1+1)
    debugr=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+5)
    lgcata=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+2)
!
    call assert(ityplo.lt.4)
!
!     1-  CAS: CHNO -> ELGA :
!     -----------------------
!     LE CAS ITYPLO=3 N EST PAS PREVU : DEVELOPPEMENT A FAIRE ...
    call assert(ityplo.ne.3)
!
!     2-  CAS: CHNO -> ELNO :
!         CAS: CHNO -> ELEM (MOYENNE)
!     --------------------------------
    if ((ityplo.eq.2) .or. (ityplo.eq.1)) then
        if (ityplo .eq. 2) then
            moyenn=.false.
        else
            moyenn=.true.
        endif
!
!
!       2.1 ON CHERCHE NNO SUR LE 1ER ELEMENT :
!       ---------------------------------------
        ima=numail(igr,1)
        call assert(ima.ne.0)
        if (ima .gt. 0) then
            nno=zi(ilmaco-1+ima+1)-zi(ilmaco-1+ima)
        else
            nno=zi(ilmsco-1-ima+1)-zi(ilmsco-1-ima)-1
        endif
!
!
!       2.2 ON RECUPERE LE DEBUT DU DESCRIPTEUR GRANDEUR :
!       --------------------------------------------------
        nbpt=zi(modloc-1+4)
        nbpt2=mod(nbpt,10000)
        if (nbpt .ne. nbpt2) then
            diff=.true.
        else
            diff=.false.
            idg2=5
        endif
!
!       MOYENN => (NBPT2=1)
        call assert((.not.moyenn) .or. (nbpt2.eq.1))
!
!       .NOT.MOYENN => (NBPT2=NNO)
        call assert(moyenn .or. (nbpt2.eq.nno))
!
!
!       2.3 SI MOYENN, IL FAUT METTRE A ZERO LE CHAMP LOCAL
!           (POUR POUVOIR CUMULER)
!       --------------------------------------------------
        if (moyenn) then
            ncmp=lgcata
            if (typegd .eq. 'R') then
                if (lparal) then
                    do 20 iel = 1, nbelgr
                        if (zl(jparal-1+iel)) then
                            iaux1=iachlo+debugr-1+(iel-1)*ncmp
                            do 10 k = 1, ncmp
                                zr(iaux1-1+k)=0.d0
10                          continue
                        endif
20                  continue
                else
                    do 30,k=1,nbelgr*ncmp
                    zr(iachlo+debugr-1-1+k)=0.d0
30                  continue
                endif
            else if (typegd.eq.'C') then
                if (lparal) then
                    do 50 iel = 1, nbelgr
                        if (zl(jparal-1+iel)) then
                            iaux1=iachlo+debugr-1+(iel-1)*ncmp
                            do 40 k = 1, ncmp
                                zc(iaux1-1+k)=(0.d0,0.d0)
40                          continue
                        endif
50                  continue
                else
                    do 60,k=1,nbelgr*ncmp
                    zc(iachlo+debugr-1-1+k)=(0.d0,0.d0)
60                  continue
                endif
            else
                call assert(.false.)
            endif
        endif
!
!
!
!        ---SI C'EST 1 CHAMP A REPRESENTATION CONSTANTE (NUM<0):
!        -------------------------------------------------------
        if (num .lt. 0) then
            long=-num
            deb2=debugr
            do 90,iel=1,nbelgr
            if (lparal) then
                if (.not.zl(jparal-1+iel)) then
                    deb2=deb2+lgcata
                    goto 90
                endif
            endif
            ima=numail(igr,iel)
            call assert(ima.ne.0)
            do 80 ino = 1, nno
                if (diff) idg2=5+nec*(ino-1)
                if (ima .gt. 0) then
                    nugl=numglm(ima,ino)
                else
                    nugl=numgls(-ima,ino)
                endif
                deb1=(nugl-1)*long+1
!
                if (nugl .gt. 0) then
                    call trigd(zi(desc-1+3), deb1, zi(modloc-1+idg2), deb2, moyenn,&
                               ino, nno)
                else
!                 ON VERIFIE QUE LE MODLOC AFFIRME NCMP=0:
                    do 70,iec=1,nec
                    if (zi(modloc-1+idg2-1+iec) .ne. 0) then
                        call u2mess('F', 'CALCULEL2_52')
                    endif
70                  continue
                endif
80          continue
90          continue
        else
!
!        --- C'EST 1 CHAMP AVEC PROFIL_NOEUD:
!        ------------------------------------
            prno1=zi(iachii-1+11*(iichin-1)+8)
            prno2=zi(iachii-1+11*(iichin-1)+9)
            deb2=debugr
            do 110,iel=1,nbelgr
            if (lparal) then
                if (.not.zl(jparal-1+iel)) then
                    deb2=deb2+lgcata
                    goto 110
                endif
            endif
            ima=numail(igr,iel)
            call assert(ima.ne.0)
            do 100 ino = 1, nno
                if (diff) idg2=5+nec*(ino-1)
                if (ima .gt. 0) then
                    nugl=numglm(ima,ino)
                else
                    nugl=numgls(-ima,ino)
                endif
                deb1=(abs(nugl)-1)*(nec+2)+1
                idg1=(abs(nugl)-1)*(nec+2)+3
!
                if (nugl .gt. 0) then
                    call trigd(zi(prno1-1+idg1), zi(prno1-1+deb1), zi(modloc-1+idg2), deb2,&
                               moyenn, ino, nno)
                else
                    call trigd(zi(prno2-1+idg1), zi(prno2-1+deb1), zi(modloc-1+idg2), deb2,&
                               moyenn, ino, nno)
                endif
100          continue
!
110          continue
        endif
!
!
        if (moyenn) then
            ncmp=lgcata
            if (typegd .eq. 'R') then
                if (lparal) then
                    do 130 iel = 1, nbelgr
                        if (zl(jparal-1+iel)) then
                            iaux1=iachlo+debugr-1+(iel-1)*ncmp
                            do 120 k = 1, ncmp
                                zr(iaux1-1+k)=zr(iaux1-1+k)/dble(nno)
120                          continue
                        endif
130                  continue
                else
                    do 140,k=1,nbelgr*ncmp
                    zr(iachlo-1+k)=zr(iachlo+debugr-1-1+k)/dble(&
                        nno)
140                  continue
                endif
            else if (typegd.eq.'C') then
                if (lparal) then
                    do 160 iel = 1, nbelgr
                        if (zl(jparal-1+iel)) then
                            iaux1=iachlo+debugr-1+(iel-1)*ncmp
                            do 150 k = 1, ncmp
                                zc(iaux1-1+k)=zc(iaux1-1+k)/dble(nno)
150                          continue
                        endif
160                  continue
                else
                    do 170,k=1,nbelgr*ncmp
                    zc(iachlo-1+k)=zc(iachlo+debugr-1-1+k)/dble(&
                        nno)
170                  continue
                endif
            else
                call assert(.false.)
            endif
        endif
!
    endif
!
!
end subroutine
