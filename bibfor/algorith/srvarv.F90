subroutine srvarv(vintr, nbmat, mater, tmp, paravi)

!
! ===================================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG             
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
! ===================================================================================

!!!
!!! MODELE LKR : CALCUL DES PARAMETRES D'ECROUISSAGE VISCO.
!!!

! ===================================================================================
! IN  : VINTR          : VARIABLE INTERNE (ICI XIV)
!     : NBMAT          : NOMBRE DE PARAMETRES DU MODELE
!     : MATER(NBMAT,2) : PARAMETRES DU MODELE
!     : TMP            : TEMPERATURE A L'INSTANT - OU +
! OUT : PARAVI(3)      : PARAMETRES D'ECROUISSAGE VISCO. (AXIV, SXIV, MXIV)
! ===================================================================================

    implicit      none
    
    !!!
    !!! Variables globales
    !!!
    
    integer :: nbmat
    real(kind=8) :: vintr,mater(nbmat,2),paravi(3),tmp
    
    !!!
    !!! Variables locales
    !!!
    
    real(kind=8) :: sxiv,axiv,mxiv,xiv
    real(kind=8) :: a0,a2,m0,s0,v1
    real(kind=8) :: fp,m1,s1,ffp,sigc,qi,fi
    real(kind=8) :: xi5,s5,m5,a5
    real(kind=8) :: qi0,m00,m10,xi50,rq,rm
    real(kind=8) :: rs,rx5,trr,dtmp
    real(kind=8) :: fact1,fact2,fact3
    
    !!!
    !!! Recuperation des parametres du modele
    !!!
    
    !!! parametres a T0
    sigc=mater(3,2)
    v1=mater(6,2)
    a0=5.0d-1
    a2=mater(8,2)
    m00=mater(9,2)
    m10=mater(10,2)
    qi0=mater(11,2)
    xi50=mater(14,2)
    fp=mater(15,2)
    rq=mater(21,2)
    rm=mater(22,2)
    rs=mater(23,2)
    rx5=mater(26,2)
    
    !!! temperatures
    trr=mater(8,1)
    
    !!! parametres a T
    if ((tmp.ge.trr).and.(trr.gt.0.d0)) then
        qi=qi0*(1.d0-rq*log(tmp/trr))
        dtmp=tmp-trr
    else
        qi=qi0
        dtmp=0.d0
    endif
    
    m0=m00*exp(-rm*(dtmp**2.d0))
    m1=m10*exp(-rm*(dtmp**2.d0))
    s1=1.d0*exp(-rs*(dtmp**2.d0))
    xi5=xi50*exp(rx5*dtmp)
    s0=(m0*1.d-1/(1.d0-1.d-1**2.d0))**2.d0
    
    !!! Si fp .ge. 0, le seuil visco. max est confondu avec le seuil initial
    if (fp.le.0.d0) then
        s5=s0
        m5=m0
        a5=a0
    else
        ffp=s0*(1.d0-fp)/m0+fp*s1/m1
        fi=qi/sigc
        fact1=ffp*m1*fi**(1.d0/a2)
        fact2=fi**2.d0-s1+m1*ffp
        !!!
        s5=fact1/fact2
        m5=s5/ffp
        a5=a2
    endif
    
    !!!
    !!! Calcul des parametres d'ecrouissage pour xiv .lt. 0
    !!!
    
    xiv=vintr
    
    if (xiv.lt.0.d0) then
        
        axiv=a0
        sxiv=s0
        mxiv=m0
    
    !!!
    !!! Calcul des parametres d'ecrouissage pour 0 .le. xiv .lt. xi5
    !!!
    
    else if ((xiv.ge.0.d0).and.(xiv.lt.xi5)) then
        
        fact3=(1.d0-xiv/xi5)**v1
        axiv=a5-(a5-a0)*fact3
        sxiv=s5-(s5-s0)*fact3
        mxiv=m5-(m5-m0)*fact3
    
    !!!
    !!! Calcul des parametres d'ecrouissage pour xiv .ge. xi5
    !!!
    
    else if (xiv.ge.xi5) then
        
        axiv=a5
        sxiv=s5
        mxiv=m5
    
    endif
    
    !!!
    !!! Stockage
    !!!
    
    paravi(1)=axiv
    paravi(2)=sxiv
    paravi(3)=mxiv

end subroutine
