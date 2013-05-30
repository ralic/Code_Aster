subroutine xls2d(callst, grille, jltsv, jltsl, jlnsv,&
                 jlnsl, nbno, jcoor, jcoorg, nbmaf,&
                 jdlima, nbsef, jdlise, jconx1, jconx2)
!
    implicit none
!
    include 'jeveux.h'
    include 'asterc/r8maem.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/padist.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    include 'blas/ddot.h'
    integer :: nbno, jcoor, jcoorg, nbmaf, nbsef, jdlima, jdlise
    integer :: jlnsv, jlnsl, jltsv, jltsl, jconx1, jconx2
    logical :: callst, grille
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
    integer :: ino, imafis, nmaabs, inoma, nuno(2), jcrd
    real(kind=8) :: p(2), dmin, a(2), b(2), m(2), ap(2), ab(2), norcab, ps, eps
    real(kind=8) :: d, oriabp, xln, ps1, xlt
    integer :: isefis, nseabs, inose, nunose, n1, nbnoma, num, nunoc, i
    logical :: ma2ff
    integer :: ir, ir2, ir3, jmafit, jmafif, jmaori, nuno1, nuno2, nunoi, ori
    logical :: finfis
!
    call jemarq()
!
    if (grille) then
        jcrd = jcoorg
    else
        jcrd = jcoor
    endif
!
!     REORGANISATION DES MAILLES DE LA FISSURE
!     VECTEURS INTERMEDIAIRE ET ORIENTATION DES MAILLES
    call wkvect('&&XINILS.LIMFISO', 'V V I', nbmaf, jmafit)
    call wkvect('&&XINILS.ORIENT', 'V V I', nbmaf, jmaori)
!     INITIALISATION PREMIERE MAILLE
    ori=1
    zi(jmaori)=1
    zi(jmafit)=zi(jdlima)
!     PARCOURS DES MAILLES CONTIGUES DANS 1 SENS
    do 114 ir = 2, nbmaf
        nunoi=zi(jconx1-1+zi(jconx2+zi(jmafit+ir-1-1)-1)+1+ori-1)
        finfis=.true.
        do 113 ir2 = 1, nbmaf
            nuno1=zi(jconx1-1+zi(jconx2+zi(jdlima+ir2-1)-1)+1-1)
            nuno2=zi(jconx1-1+zi(jconx2+zi(jdlima+ir2-1)-1)+2-1)
            if (zi(jdlima+ir2-1) .ne. zi(jmafit+ir-1-1) .and.&
                ( nunoi.eq.nuno1 .or. nunoi.eq.nuno2)) then
                if (nunoi .eq. nuno1) ori=1
                if (nunoi .eq. nuno2) ori=0
                zi(jmaori+ir-1)=ori
                zi(jmafit+ir-1)=zi(jdlima+ir2-1)
                finfis=.false.
                goto 1135
            endif
113      continue
1135      continue
        if (finfis) goto 1145
114  end do
1145  continue
!
!     VECTEUR FINAL REORGANISE
    call wkvect('&&XINILS.LIMFISOF', 'V V I', nbmaf, jmafif)
!     DECALAGE DES MAILLES TROUVEES
    do 115 ir3 = 1, ir-1
        zi(jmafif+nbmaf-ir+ir3)=zi(jmafit-1+ir3)
115  end do
!     PARCOURS DANS L'AUTRE SENS A PARTIR DE LA PREMIERE MAILLE
    ori=0
    do 117 ir2 = ir, nbmaf+1
        nunoi=zi(jconx1-1+zi(jconx2+zi(jmafif+nbmaf-ir2+1)-1)+1+ori-1)
        finfis=.true.
        do 116 ir3 = 1, nbmaf
            nuno1=zi(jconx1-1+zi(jconx2+zi(jdlima+ir3-1)-1)+1-1)
            nuno2=zi(jconx1-1+zi(jconx2+zi(jdlima+ir3-1)-1)+2-1)
            if (zi(jdlima+ir3-1) .ne. zi(jmafif+nbmaf-ir2+1) .and.&
                ( nunoi.eq.nuno1 .or. nunoi.eq.nuno2)) then
                if (nunoi .eq. nuno1) ori=1
                if (nunoi .eq. nuno2) ori=0
                zi(jmaori+nbmaf-ir2)=ori
                zi(jmafif+nbmaf-ir2)=zi(jdlima+ir3-1)
                finfis=.false.
                goto 1165
            endif
116      continue
1165      continue
        if (finfis) goto 1175
117  end do
1175  continue
!
!      DO 118 IR3=1,NBMAF
!      WRITE (6,*) IR3, ZI(JCONX1-1+ZI(JCONX2+ZI(JDLIMA+IR3-1)-1)+1-1)
!     & , ZI(JCONX1-1+ZI(JCONX2+ZI(JDLIMA+IR3-1)-1)+2-1)
! 118  CONTINUE
!      DO 1182 IR3=1,NBMAF
!      WRITE (6,*) IR3, ZI(JCONX1-1+ZI(JCONX2+ZI(JMAFIF+IR3-1)-1)+1-1)
!     & , ZI(JCONX1-1+ZI(JCONX2+ZI(JMAFIF+IR3-1)-1)+2-1)
!     & , ZI(JMAORI-1+IR3)
! 1182 CONTINUE
!
    if (ir2-1 .ne. nbmaf) then
        write (6,*) ir,ir2,nbmaf
!     MAILLES MANQUANTES
        call assert(ir2-1.eq.nbmaf)
    endif
!
!     BOUCLE SUR LES NOEUDS P DU MAILLAGE
    do 11 ino = 1, nbno
        p(1)=zr(jcrd-1+3*(ino-1)+1)
        p(2)=zr(jcrd-1+3*(ino-1)+2)
!
!     CALCUL DE LSN
!     -------------
        dmin=r8maem()
!         RECHERCHE DE LA MAILLE LA PLUS PROCHE :
!         BOUCLE SUR NOEUDS DE MAFIS
        do 2 imafis = 1, nbmaf
            nmaabs=zi(jmafif-1+(imafis-1)+1)
            inoma=1
            nuno(inoma)=zi(jconx1-1+zi(jconx2+nmaabs-1)+inoma-1)
            a(1)=zr(jcoor-1+3*(nuno(inoma)-1)+1)
            a(2)=zr(jcoor-1+3*(nuno(inoma)-1)+2)
!
            inoma=2
            nuno(inoma)=zi(jconx1-1+zi(jconx2+nmaabs-1)+inoma-1)
            b(1)=zr(jcoor-1+3*(nuno(inoma)-1)+1)
            b(2)=zr(jcoor-1+3*(nuno(inoma)-1)+2)
!
            do 211 i = 1, 2
                ab(i)=b(i)-a(i)
                ap(i)=p(i)-a(i)
211          continue
!
!           CALCUL DE EPS TEL QUE AM=EPS*AB
            norcab=ab(1)*ab(1)+ab(2)*ab(2)
            ps=ddot(2,ap,1,ab,1)
            eps=ps/norcab
!
!           ON RAMENE LES POINTS EN DEHORS DU SEGMENT
            if (eps .lt. 0.d0) eps=0.d0
            if (eps .gt. 1.d0) eps=1.d0
!
            do 212 i = 1, 2
                m(i)=a(i)+eps*ab(i)
212          continue
!
!           CALCUL DE LA DISTANCE PM
            d=padist(2,p,m)
!
!           MISE EN MEMOIRE DE LSN POUR LA MAILLE LA PLUS PROCHE
!           EN VERIFIANT DE QUEL COTE DE LA FISSURE SE TROUVE P
            if (d .lt. dmin) then
                dmin=d
                oriabp=ab(1)*ap(2)-ab(2)*ap(1)
                do 213 i = 1, 2
                    m(i)=a(i)+ps/norcab*ab(i)
213              continue
                d=padist(2,p,m)
                if (oriabp .gt. 0.d0) then
                    xln=d
                else
                    xln=-1.d0*d
                endif
                if (zi(jmaori-1+imafis) .eq. 0) then
                    xln=-1.d0*xln
                endif
            endif
!
 2      continue
!
        zr(jlnsv-1+(ino-1)+1)=xln
        zl(jlnsl-1+(ino-1)+1)=.true.
!
!        CALCUL DE LST
!        -------------
!
        if (.not.callst) then
            xlt = -1.d0
            goto 888
        endif
!
        dmin=r8maem()
!
!         RECHERCHE DU POINT LE PLUS PROCHE : BOUCLE SUR POINT DE FONFIS
        do 3 isefis = 1, nbsef
!
            nseabs=zi(jdlise-1+(isefis-1)+1)
            inose=1
            nunose=zi(jconx1-1+zi(jconx2+nseabs-1)+inose-1)
!
!           BOUCLE SUR LES MAILLES DE MAFIS POUR TROUVER LA BONNE MAILLE
            ma2ff=.false.
            do 31 imafis = 1, nbmaf
!
                nmaabs=zi(jmafif-1+(imafis-1)+1)
                nbnoma=zi(jconx2+nmaabs) - zi(jconx2+nmaabs-1)
!             ON RECUPERE LES NUMEROS DS NOEUDS DE LA MAILLE ET ON TESTE
                n1=0
!
                do 32 inoma = 1, nbnoma
                    num=zi(jconx1-1+zi(jconx2+nmaabs-1)+inoma-1)
                    if (nunose .eq. num) n1=1
!               POUR RECUPERER UN 2EME POINT DE LA MAILLE QUI NE SOIT
!               PAS SUR LE FOND
                    if ((nunose.ne.num)) nunoc=num
32              continue
!
                if (n1 .eq. 1) then
!
                    ma2ff=.true.
                    do 33 i = 1, 2
                        a(i)=zr(jcoor-1+3*(nunose-1)+i)
                        b(i)=zr(jcoor-1+3*(nunoc-1)+i)
                        ab(i)=b(i)-a(i)
                        ap(i)=p(i)-a(i)
33                  continue
!
!               PROJECTION SUR LE SEGMENT
                    ps=ddot(2,ap,1,ab,1)
                    ps1=ddot(2,ab,1,ab,1)
                    eps=ps/ps1
!
!               CALCUL DE LA DISTANCE PA
                    d=padist(2,p,a)
!               MISE EN MEMOIRE DE LSN=PA.N POUR LE SEG LE PLUS PROCHE
                    if (d .lt. dmin) then
                        dmin=d
                        xlt=-1.d0*eps*sqrt(ab(1)*ab(1)+ab(2)*ab(2))
                    endif
!
                endif
!
31          continue
!
            if (.not.ma2ff) call u2mess('F', 'XFEM2_15')
 3      continue
!
888      continue
        zr(jltsv-1+(ino-1)+1)=xlt
        zl(jltsl-1+(ino-1)+1)=.true.
!
11  continue
!
    call jedema()
!
end subroutine
