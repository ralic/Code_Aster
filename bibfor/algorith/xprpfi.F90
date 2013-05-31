subroutine xprpfi(p, lsnp, lcmin, poifis, trifis,&
                  fiss, ndim, lsn, lst)
! aslint: disable=W1501
    implicit none
    include 'jeveux.h'
    include 'asterc/r8maem.h'
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/xprali.h'
    include 'asterfort/xproj.h'
    real(kind=8) :: p(3), lsnp, lcmin, lsn, lst
    character(len=19) :: poifis, trifis
    character(len=8) :: fiss
    integer :: ndim
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
! person_in_charge: daniele.colombo at ifpen.fr
!
!     ------------------------------------------------------------------
!
!       XPRPFI   : X-FEM PROPAGATION : CALCUL DE LA PROJECTION D'UN
!       ------     -     --                         -
!                  POINT SUR LA SURFACE DE LA FISSURE
!                                             --
!    DANS LE CADRE DE LA PROPAGATION X-FEM, ON CALCULE LA VALEUR DES
!    LEVEL SETS NORMALE ET TANGENTIELLE POUR UN POINT EN UTILISANT LEUR
!    DEFINITION. ON CALCULE DONC LA PROJECTION DU POINT SUR LA SURFACE
!    DE LA FISSURE.
!
!    ENTREE
!        P      = COORDONNEES DU POINT A PROJETER
!        LSNP   = VALEUR ACTUELLE DE LSN (OU LST SI ON REINITIALISE LA
!                 LEVEL SET TANGENTIELLE)
!        LCMIN  = LONGUEUR DE LA PLUS PETITE ARETE DU MAILLAGE OU DE LA
!                 GRILLE AUXILIAIRE
!        POIFIS = NOM DE L'OBJET JEVEUX OU LA LISTE DES COORDONNEES DES
!                 POINTS D'INTERSECTION ENTRE LES ELEMENTS ET LSN=0
!                 EST STOCKEE (VOIR XPRLS0.F)
!        TRIFIS = NOM DE L'OBJET JEVEUX OU LE NUMERO ET LA LISTE DES
!                 POINTS D'INTERSECTION ENTRE LES ELEMENTS ET LSN=0 SONT
!                 STOCKES (VOIR XPRLS0.F)
!        FISS   = NOM DU CONCEPT FISSURE X-FEM
!        NDIM   = DIMENSION DU MODELE (2=2D, 3=3D)
!
!    SORTIE
!        LSN    = VALEUR DE LA LEVEL SET NORMALE AU POINT P
!        LST    = VALEUR DE LA LEVEL SET TANGENTIELLE AU POINT P
!
!    DANS LE CAS OU ON CALCULE LA REINITIALISATION DE LST, ON DONNE EN
!    ENTRE LA LST (PARAMETRE LSNP) ET ON OBTIENT LA VALEUR DE LST
!    RECALCULEE EN SORTIE (PARAMETRE LSN) ET LA VALEUR DE LSN (PARAMETRE
!    LST).
!
!     ------------------------------------------------------------------
!
!
!     GENERAL PURPOSE
    integer :: ifm, niv, i
    character(len=1) :: k1bid
!
!     UPWIND INTEGRATION
    integer :: jtri, jpoi, elcut, ntri, itri, ia, ib, ic, nptint, pa, pb, pc
    integer :: psx, pdx, pter, nelcou, np, jfonf, jfmult, numpon
    real(kind=8) :: a(3), b(3), c(3), lsta, lstb, lstc, m(3), d, vn(3), eps(3)
    real(kind=8) :: bestd, mp(3), v(3), vnele(3), vin(3), pm(3), pmp(3), toll
    real(kind=8) :: d1
    logical :: in, eps1z, eps2z, eps3z, eps1u, eps2u, mvert, libre, mpin, kink
!
!  TRIANGLES ABC QUE L'ON PEUT FORMER A PARTIR DE N POINTS (N=3 A 6)
    integer :: iatri(20), ibtri(20), ictri(20)
!        ---------------------
!        |  I | TRIANGLE | N |
!        --------------------
!        |  1 |   1 2 3  | 3 |
!        --------------------
!        |  2 |   1 2 4  |   |
!        |  3 |   1 3 4  | 4 |
!        |  4 |   2 3 4  |   |
!        --------------------
!        |  5 |   1 2 5  |   |
!        |  6 |   1 3 5  |   |
!        |  7 |   1 4 5  | 5 |
!        |  8 |   2 3 5  |   |
!        |  9 |   2 4 5  |   |
!        | 10 |   3 4 5  |   |
!        --------------------
!        | 11 |   1 2 6  |   |
!        | 12 |   1 3 6  |   |
!        | 13 |   1 4 6  |   |
!        | 14 |   1 5 6  |   |
!        | 15 |   2 3 6  | 6 |
!        | 16 |   2 4 6  |   |
!        | 17 |   2 5 6  |   |
!        | 18 |   3 4 6  |   |
!        | 19 |   3 5 6  |   |
!        | 20 |   4 5 6  |   |
!        --------------------
    data   iatri/1,1,1,2,1,1,1,2,2,3,1,1,1,1,2,2,2,3,3,4/
    data   ibtri/2,2,3,3,2,3,4,3,4,4,2,3,4,5,3,4,5,4,5,5/
    data   ictri/3,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6/
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
!     INITIALISE THE VALUE OF THE DISTANCE
    bestd = r8maem()
!
!     INITIALISE THE FLAG USED TO ASSESS IF THE POINT IS INSIDE THE
!     TRIANGLE AND CALCULATE THE TOLERANCE
    mvert = .false.
    toll = 1.d-2*lcmin
!
!     RETRIEVE THE VECTORS CONTAINING THE TRIANGULATION OF LSN=0
    call jeveuo(poifis, 'L', jpoi)
    call jeveuo(trifis, 'L', jtri)
!
!     RETRIEVE THE NUMBER OF ELEMENTS CUT BY THE LSN=0
    call jelira(trifis, 'LONMAX', elcut, k1bid)
    elcut = elcut/7
!
!     ******************************************************************
!     CALCULATE THE (INNER OR OUTER) PROJECTION OF THE POINT P ON THE
!     TRIANGULATION DEFINING THE LSN=0
!     ******************************************************************
!
!     LOOP ON EACH ELEMENT CUT BY THE LSN=0. THE SAME ALGORITHM CODED
!     IN XPRLS0.F IS USED HERE.
    do 1000 i = 1, elcut
!
!        RETREIVE THE NUMBER OF INTERSECTION POINTS FOR THE ELEMENT
        nptint = zi(jtri-1+7*(i-1)+1)
!
!        DETERMINE THE NUMBER OF TRIANGLES THAT CAN BE DEFINED
        if (nptint .eq. 3) ntri=1
        if (nptint .eq. 4) ntri=4
        if (nptint .eq. 5) ntri=10
        if (nptint .eq. 6) ntri=20
!
!        LOOP ON EACH TRIANGLE TO CALCULATE THE PROJECTION POINT AND
!        THE NORMAL DISTANCE OF THE POINT TO THE LSN=0
        do 350 itri = 1, ntri
            ia = iatri(itri)
            ib = ibtri(itri)
            ic = ictri(itri)
!
!           RETREIVE THE POSITION OF THE THREE POINTS A,B,C OF THE
!           TRIANGLE IN THE COORDINATES TABLE
            pa = zi(jtri-1+7*(i-1)+ia+1)
            pb = zi(jtri-1+7*(i-1)+ib+1)
            pc = zi(jtri-1+7*(i-1)+ic+1)
!
!           RETREIVE THEIR COORDINATES
            a(1) = zr(jpoi-1+4*(pa-1)+1)
            a(2) = zr(jpoi-1+4*(pa-1)+2)
            a(3) = zr(jpoi-1+4*(pa-1)+3)
!
            b(1) = zr(jpoi-1+4*(pb-1)+1)
            b(2) = zr(jpoi-1+4*(pb-1)+2)
            b(3) = zr(jpoi-1+4*(pb-1)+3)
!
            c(1) = zr(jpoi-1+4*(pc-1)+1)
            c(2) = zr(jpoi-1+4*(pc-1)+2)
            c(3) = zr(jpoi-1+4*(pc-1)+3)
!
!           RETREIVE THEIR LST
            lsta = zr(jpoi-1+4*(pa-1)+4)
            lstb = zr(jpoi-1+4*(pb-1)+4)
            lstc = zr(jpoi-1+4*(pc-1)+4)
!
!           CALCULATE THE (CORRECTED AND NOT) PROJECTION AND DISTANCE
            call xproj(p, a, b, c, m,&
                       mp, d, vn, eps, in)
!
!           SEARCH FOR THE TRIANGLE THAT MINIMISE THE DISTANCE
            if (d .lt. bestd) then
!
!              STORE THE DISTANCE, THE LST AND THE "IN/OUT" FLAG
                bestd = d
                lst = eps(1)*lstb + eps(2)*lstc + eps(3)*lsta
!
!              STORE THE POINTS DEFINING THE EDGE TO WHICH THE
!              PROJECTED POINT M BELONGS (IF A FURTHER CORRECTION WILL
!              BE NECESSARY)
                if (.not.in) then
!
!                 ANALYZE THE PARAMETRIC COORDINATES EPS1,EPS2 AND EPS3
                    if (eps(1) .lt. r8prem()) then
                        eps1z = .true.
                    else
                        eps1z = .false.
                    endif
!
                    if (eps(2) .lt. r8prem()) then
                        eps2z = .true.
                    else
                        eps2z = .false.
                    endif
!
                    if (eps(3) .lt. r8prem()) then
                        eps3z = .true.
                    else
                        eps3z = .false.
                    endif
!
                    if (abs(eps(1)-1) .lt. r8prem()) then
                        eps1u = .true.
                    else
                        eps1u = .false.
                    endif
!
                    if (abs(eps(2)-1) .lt. r8prem()) then
                        eps2u = .true.
                    else
                        eps2u = .false.
                    endif
!
!                 LOCATE THE EDGE USING THE PARAMETRIC COORDINATES AND
!                 STORE ALL THE DATA THAT WILL BE EVENTUALLY USED FOR
!                 FURTHER CALCULATIONS
                    if (eps1z .and. (.not.eps2z) .and. (.not.eps2u)) then
!                    THE POINT M IS ON EDGE A-C OF THE TRIANGLE
                        psx = pa
                        pdx = pc
                        pter = pb
                        nelcou = i
                        pm(1) = m(1)
                        pm(2) = m(2)
                        pm(3) = m(3)
                        pmp(1) = mp(1)
                        pmp(2) = mp(2)
                        pmp(3) = mp(3)
                        vnele(1) = vn(1)
                        vnele(2) = vn(2)
                        vnele(3) = vn(3)
                        mvert = .false.
!
                        else if (eps2z.and.(.not.eps1z).and.(.not.eps1u))&
                    then
!                    THE POINT M IS ON EDGE A-B OF THE TRIANGLE
                        psx = pa
                        pdx = pb
                        pter = pc
                        nelcou = i
                        pm(1) = m(1)
                        pm(2) = m(2)
                        pm(3) = m(3)
                        pmp(1) = mp(1)
                        pmp(2) = mp(2)
                        pmp(3) = mp(3)
                        vnele(1) = vn(1)
                        vnele(2) = vn(2)
                        vnele(3) = vn(3)
                        mvert = .false.
!
                        else if (eps3z.and.(.not.eps1z).and.(.not.eps1u)&
                    .and. (.not.eps2z).and.(.not.eps2u)) then
!                    THE POINT M IS ON EDGE B-C OF THE TRIANGLE
                        psx = pb
                        pdx = pc
                        pter = pa
                        nelcou = i
                        pm(1) = m(1)
                        pm(2) = m(2)
                        pm(3) = m(3)
                        pmp(1) = mp(1)
                        pmp(2) = mp(2)
                        pmp(3) = mp(3)
                        vnele(1) = vn(1)
                        vnele(2) = vn(2)
                        vnele(3) = vn(3)
                        mvert = .false.
!
                    else
!                    THE POINT M IS COINCIDENT WITH ONE OF THE THREE
!                    NODES. THIS IS THE MOST COMPLICATED CASE AND IT
!                    WILL SUBJECTED TO FURTHER CALCULATIONS IF NECESSARY
                        if (eps1z .and. eps2z) then
!                       POINT M = POINT A
                            psx = pa
                            pdx = pb
                            pter = pc
                            nelcou = i
                            pm(1) = m(1)
                            pm(2) = m(2)
                            pm(3) = m(3)
                            pmp(1) = mp(1)
                            pmp(2) = mp(2)
                            pmp(3) = mp(3)
                            vnele(1) = vn(1)
                            vnele(2) = vn(2)
                            vnele(3) = vn(3)
                            mvert = .true.
                        else if (eps1z.and.eps2u) then
!                       POINT M = POINT C
                            psx = pc
                            pdx = pa
                            pter = pb
                            nelcou = i
                            pm(1) = m(1)
                            pm(2) = m(2)
                            pm(3) = m(3)
                            pmp(1) = mp(1)
                            pmp(2) = mp(2)
                            pmp(3) = mp(3)
                            vnele(1) = vn(1)
                            vnele(2) = vn(2)
                            vnele(3) = vn(3)
                            mvert = .true.
                        else if (eps1u.and.eps2z) then
!                       POINT M = POINT B
                            psx = pb
                            pdx = pc
                            pter = pa
                            nelcou = i
                            pm(1) = m(1)
                            pm(2) = m(2)
                            pm(3) = m(3)
                            pmp(1) = mp(1)
                            pmp(2) = mp(2)
                            pmp(3) = mp(3)
                            vnele(1) = vn(1)
                            vnele(2) = vn(2)
                            vnele(3) = vn(3)
                            mvert = .true.
                        else
!
!                       THIS CASE DOESN'T EXIST. FOR SAFETY IT'S BETTER
!                       TO STOP THE CODE IF WE WILL ARRIVE IN THIS POINT
!                       OF THE SUBROUTINE
                            call assert(1.gt.2)
!
                        endif
!
                    endif
!
                else
!
!                 IF THE PROJECTION IS INSIDE THE TRIANGLE, THERE'S NO
!                 NEED TO STORE THE POINTS DEFINING THE EDGE OF THE
!                 TRIANGLE TO WHICH THE PROJECTION BELONGS
                    mvert = .false.
                    psx = 0
!
                endif
!
            endif
!
350      continue
!
1000  end do
!
!     ******************************************************************
!     IN ORDER TO CORRECTLY CALCULATE LSN, THE NORMAL DISTANCE TO THE
!     PLANE OF THE SELECTED TRIANGLE IS CALCULATED IF THE POINT
!     PROJECTION IS OUTSIDE THE DOMAIN OF THE CALCULUS.
!     ******************************************************************
!
!     CASE 1: THE NORMAL PROJECTION IS OUTSIDE THE TRIANGLE AND
!             THE CORRECTED PROJECTION BELONGS TO AN EDGE OF THE
!             TRIANGLE
    if ((.not.mvert) .and. (psx.gt.0) .and. (pdx.gt.0) .and. (pter.gt.0)) then
!
!         CALCULATE THE DISTANCE BETWEEN THE PROJECTION POINT (OUTSIDE)
!         AND THE CORRECTED PROJECTION POINT (ON THE EDGE)
        d=sqrt((pm(1)-pmp(1))**2+(pm(2)-pmp(2))**2+(pm(3)-pmp(3))**2)
!
!         IF THE DISTANCE IS LOWER THAN THE TOLERANCE, THE TWO POINTS
!         ARE CONSIDERED COINCIDENT AND THE CALCULATED NORMAL DISTANCE
!         IS ACCEPTED
        if (d .gt. toll) then
!
!            YES, THE PROJECTION IS EFFECTIVELY OUTSIDE THE TRIANGLE.
!            VERIFY IF THE EDGE OF THE TRIANGLE IS SHARED BY ANY OTHER
!            TRIANGLE BELONGING TO ANOTHER ELEMENT TO ASSESS IF THE
!            PROJECTION IS OUTSIDE THE DOMAIN.
            call xprali(psx, pdx, vnele, nelcou, poifis,&
                        trifis, libre, vin)
!
            if (libre) then
!               YES, THE EDGE IS A FREE EDGE AND THEREFORE THE
!               PROJECTION IS ALSO OUTSIDE THE DOMAIN.
!               THE NORMAL DISTANCE MUST BE CALCULATED.
                bestd = sqrt( (p(1)-pmp(1))**2+(p(2)-pmp(2))**2+ (p(3)-pmp(3))**2)
            endif
!
        endif
!
!     CASE 2: THE NORMAL PROJECTION IS OUTSIDE THE TRIANGLE AND
!             THE CORRECTED PROJECTION IS COINCIDENT WITH ONE OF THE
!             THREE POINTS DEFINING THE TRIANGLE. THIS IS THE MOST
!             COMPLICATED CASE.
    else if (mvert.and.(ndim.eq.3)) then
!
!          RETREIVE THE COORDINATES OF THE POINT OF THE TRIANGLE
        b(1) = zr(jpoi-1+4*(psx-1)+1)
        b(2) = zr(jpoi-1+4*(psx-1)+2)
        b(3) = zr(jpoi-1+4*(psx-1)+3)
!
!          CALCULATE THE DISTANCE BETWEEN THE PROJECTED POINT AND
!          THE POINT OF THE TRIANGLE ABOVE
        d = sqrt((b(1)-pmp(1))**2+(b(2)-pmp(2))**2+(b(3)-pmp(3))**2)
!
!          IF THEY ARE NOT COINCIDENT, WE CAN PROCEED WITH THE
!          CALCULATIONS
        if (d .gt. r8prem()) then
!
!             IN THE CASE THE PROJECTED POINT PSX IS COINCIDENT WITH
!             ONE OF THE END OF THE CRACK FRONT, A CORRECTION MUST BE
!             ADOPTED IN ORDER TO PREVENT A DEFORMATION OF THE LEVEL
!             SET IN PRESENCE OF KINKS.
            kink = .false.
!
!             RETREIVE THE COORDINATES OF THE POINT OF THE CRACK FRONT
            call jeveuo(fiss//'.FONDFISS', 'L', jfonf)
!
!             RETREIVE THE END POINTS OF EACH PART OF THE CRACK FRONT
            call jeveuo(fiss//'.FONDMULT', 'L', jfmult)
            call jelira(fiss//'.FONDMULT', 'LONMAX', numpon, k1bid)
!
!             LOOP ON THE END POINTS OF THE FRONT TO CHECK THE "KINK
!             CONDITION"
            do 1500 i = 1, numpon
!
!                RETREIVE THE COORDINATES OF THE POINT
                a(1) = zr(jfonf-1+4*(i-1)+1)
                a(2) = zr(jfonf-1+4*(i-1)+2)
                a(3) = zr(jfonf-1+4*(i-1)+3)
!
!                CALCULATE THE DISTANCE
                d = sqrt((a(1)-b(1))**2+(a(2)-b(2))**2+(a(3)-b(3))**2)
!
                if (d .lt. r8prem()) then
                    kink = .true.
                    goto 1600
                endif
!
1500          continue
!
!             CONTINUE WITH THE CALCULATIONS. THE KINK CONDITION WILL
!             BE CONSIDERED LATER.
1600          continue
!
!             RETREIVE THE NUMBER OF INTERSECTION POINTS FOR THE
!             ELEMENT
            nptint = zi(jtri-1+7*(nelcou-1)+1)
!
!             FLAG=PROJECTION INSIDE THE TRIANGLE
            mpin = .true.
!
!             SEARCH FOR THE FREE EDGES (OF ANY TRIANGLE) STARTING FROM
!             THE PROJECTION NODE. NO MORE THAN TWO FREE EDGES ARE
!             EXPECTED
            do 2000 i = 1, nptint
!
!                RETREIVE THE I-TH INTERSECTION POINT OF THE ELEMENT
                np = zi(jtri-1+7*(nelcou-1)+i+1)
!
                if (np .ne. psx) then
!
!                   CHECK IF THE EDGE IS ON THE FREE SURFACE
                    call xprali(psx, np, vnele, nelcou, poifis,&
                                trifis, libre, vin)
!
!                   YES...
                    if (libre) then
!                      RETREIVE THE COORDINATES OF THE PROJECTED POINT
                        a(1) = pmp(1)
                        a(2) = pmp(2)
                        a(3) = pmp(3)
!                      CALCULATE THE VECTOR CONNECTING THIS LAST POINT
!                      AND THE POINT OF THE TRIANGLE
                        v(1) = a(1)-b(1)
                        v(2) = a(2)-b(2)
                        v(3) = a(3)-b(3)
!                      CALCULATE THE SCALAR PRODUCT BETWEEN THIS VECTOR
!                      AND THE NORMAL TO THE FREE SURFACE OF THE DOMAIN,
!                      THAT IS THE DISTANCE BETWEEN THE PROJECTED POINT
!                      AND THE FREE SURFACE
                        d = v(1)*vin(1)+v(2)*vin(2)+v(3)*vin(3)
!                      A NEGATIVE VALUE MEANS THAT THE POINT IS OUTSIDE
!                      THE DOMAIN (A TOLERANCE IS USED AS USUAL)
                        if ((d.lt.0.d0) .and. (abs(d).gt.toll)) then
                            mpin=.false.
                            goto 2100
                        endif
                    endif
!
                endif
!
2000          continue
!
2100          continue
!
!             IF THE POINT IS EFFECTIVELY OUTSIDE THE DOMAIN, CALCULATE
!             THE DISTANCE BETWEEN POINT P AND THE PROJECTION POINT
            if (.not.mpin) then
!
                d1 = sqrt((p(1)-pmp(1))**2+(p(2)-pmp(2))**2+ (p(3)- pmp(3))**2)
!
!                IN THE CASE OF KINK, A MEAN VALUE IS CALCULATED
                if (kink) then
                    bestd = (bestd+d1)/2.d0
                else
                    bestd = d1
                endif
!
            endif
!
        endif
!
!     CASE 3: AS CASE 2 BUT WE ARE WORKING ON A 2D MODEL. EVERYTHING IS
!             SIMPLER!
    else if (mvert.and.(ndim.eq.2)) then
!
!          FLAG TO INDICATE IF THE PROJECTION POINT IS INSIDE THE CRACK
!          SURFACE
        mpin = .false.
!
!          WE SHOULD CHECK IF PSX POINT IS SHARED BY ANY OTHER SEGMENT
!          DEFINING THE CRACK SURFACE
        do 3000 i = 1, elcut
!
!             DO NOT CHECK THE ELEMENT TO WHICH PSX BELONGS
            if (i .ne. nelcou) then
!
!                CHECK IF ANY OF THE TWO INTERSECTION POINTS IS
!                COINCIDENT WITH PSX
                if ((zi(jtri-1+7*(i-1)+2).eq.psx) .or. (zi(jtri-1+7*(i- 1)+3).eq.psx)) then
                    mpin = .true.
                    goto 3100
                endif
!
            endif
!
3000      continue
!
3100      continue
!
!          IF THE POINT IS EFFECTIVELY OUTSIDE THE DOMAIN, CALCULATE
!          THE DISTANCE BETWEEN POINT P AND THE PROJECTION POINT
        if (.not.mpin) then
            bestd = sqrt((p(1)-pmp(1))**2+(p(2)-pmp(2))**2+ (p(3)-pmp( 3))**2)
        endif
!
    endif
!
!     CALCULATED THE CORRECT NORMAL DISTANCE WITH THE CORRECT SIGN
    lsn = bestd * sign(1.d0,lsnp)
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
