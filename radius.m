function out=radius(n_dim, fskip, n1, n2)

fnm='out'

sizex = 256;
sizey = 256;

for i=1:sizex
    x(i)=i-1; %physical size in x(left/right in code) 
end; 
for j=1:sizey
    y(j)=j-1; %physical size in y (up/down in code)
end;

ymin=0;
ymax=sizey-1; %relevant physical size in up/down dir
xmin=0;
xmax=sizex-1; %rellavent physical size in left/right dir
if(n_dim==2)
 zmin=-0.025;
 zmax=-0.025;
else
  zmin=0;
  zmax=1;
end;


for f_num=n1:fskip:n2

    %"fch" string is the file being opened
    fch=[fnm,'_',num2str(f_num)];
    fich=load(fch);
    %assign file to array "den"
    den=fich(:,n_dim);

    rho=reshape(den, [sizey sizex]);
    i = (f_num- n1)/fskip;
    
    r(i+1) = sqrt(sum(sum(1 - rho))/(pi));
   
end
    plot(r, 'o')
end

